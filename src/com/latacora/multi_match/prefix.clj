(ns com.latacora.multi-match.prefix
  "Literal-prefix dispatch for whole-string matching against every named pattern.
  The trie filters candidates; original Java patterns determine matches/captures."
  (:require [com.rpl.specter :as sr]
            [lambdaisland.regal :as regal]
            [lambdaisland.regal.parse :as rparse])
  (:import (java.util.regex Pattern)))

(defn- concatenation? [form]
  (and (vector? form) (contains? #{:cat :capture} (first form))))

(def ^:private CONCAT-LEAVES
  ;; Captures and concatenations consume their children sequentially. Other
  ;; operators must remain opaque: walking into :? would make optional text
  ;; look mandatory, and walking into :alt would concatenate alternatives.
  (sr/recursive-path [] p
    (sr/if-path concatenation? [(sr/view rest) sr/ALL p] sr/STAY)))

(defn literal-prefix
  "A conservative mandatory literal prefix of a Regal form.
  Stops at the first nonliteral leaf, including alternation and repetition."
  [form]
  (apply str (take-while string? (sr/select CONCAT-LEAVES form))))

(defn- pattern-prefix [^Pattern pattern]
  ;; This Regal version discards scoped flags. Conservatively skip all (?...
  ;; constructs, and all externally/implicitly flagged patterns. Unsupported
  ;; parser constructs also disable the optimization, not Java matching.
  (if (or (not (zero? (.flags pattern)))
          (.contains (.pattern pattern) "(?"))
    ""
    (try
      (literal-prefix (rparse/parse pattern))
      (catch Exception _ ""))))

(defn- entry [[name pattern-ish]]
  (let [pattern (cond
                  (instance? Pattern pattern-ish) pattern-ish
                  (string? pattern-ish) (Pattern/compile pattern-ish)
                  :else (regal/regex pattern-ish))]
    {:name name
     :pattern pattern
     :prefix (if (or (instance? Pattern pattern-ish) (string? pattern-ish))
               (pattern-prefix pattern)
               (literal-prefix pattern-ish))}))

(defn compile-matcher
  "Compile a map or ordered sequence of [name regex] pairs for all-match lookup.
  Accepts Java Patterns, regex strings, or Regal forms. Preserves input order.
  The returned immutable matcher is safe to share between threads."
  [named-patterns]
  (let [entries (mapv entry named-patterns)
        trie (reduce-kv
              (fn [trie id {:keys [prefix]}]
                ;; A path per UTF-16 code unit also handles supplementary
                ;; characters without conflating them with Java regex atoms.
                (let [path (conj (vec (mapcat #(vector :children (sr/keypath %))
                                            prefix))
                                 :ids)]
                  (sr/transform path #(conj (or % []) id) trie)))
              {} entries)]
    {::entries entries ::trie trie}))

(defn candidate-ids
  "Return candidate IDs in input-pattern order. Candidates still need matching."
  [{::keys [trie]} ^String input]
  (loop [node trie offset 0 ids []]
    (let [ids (into ids (:ids node))]
      (if-let [child (when (< offset (.length input))
                      (get-in node [:children (.charAt input offset)]))]
        (recur child (inc offset) ids)
        (vec (sort ids))))))

(defn all-matches
  "Return every whole-string match with its name, entire match, and captures.
  Captures retain Java semantics, including nil for nonparticipating groups."
  [{::keys [entries] :as compiled} ^String input]
  (into []
        (keep (fn [id]
                (let [{:keys [name pattern]} (nth entries id)
                      matcher (.matcher ^Pattern pattern input)]
                  (when (.matches matcher)
                    {:pattern-name name
                     :entire-match (.group matcher)
                     :groups (mapv #(.group matcher (int %))
                                   (range 1 (inc (.groupCount matcher))))}))))
        (candidate-ids compiled input)))
