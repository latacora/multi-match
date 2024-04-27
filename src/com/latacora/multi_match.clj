(ns com.latacora.multi-match
  (:require
   [lambdaisland.regal :as regal]
   [lambdaisland.regal.parse :as rparse]
   [eidolon.core :as ei]
   [com.rpl.specter :as sr])
  (:import
   (java.util.regex Pattern)))

;; While Java does support named groups, that gets annoying if you have _nested_
;; capturing groups, so this entire namespace is built on traditional group
;; counting.

(defn ^:private ->regal-form
  "Given a regex string or Pattern, returns a regal form."
  [x]
  (condp instance? x
    Pattern (rparse/parse x)
    String (rparse/parse-pattern x)
    x))

(defn ^:private count-captures
  "Given a regal form, returns the number of capturing groups in it."
  [r]
  (->> r (sr/select [ei/TREE-LEAVES #{:capture}]) count))

(defn compile-multi-pattern
  "Given a map of regex names to regexes (which may be regex strings or regal
  forms or Pattern objects), compiles a multi-pattern that lets you match
  against all regexes simultaneously (but still learning what pattern matched)."
  [named-regexes]
  (let [mp (reduce
            (fn [multi-pattern [re-name pattern-ish]]
              (let [regal-form (->regal-form pattern-ish)
                    n-captures (count-captures regal-form)]
                (->
                 multi-pattern
                 (update ::multi-regal conj regal-form)
                 (update ::group-indexes assoc (::last-group-index multi-pattern) re-name)
                 (update ::last-group-index + n-captures))))
            {::multi-regal [:alt]
             ::group-indexes (sorted-map)
             ::last-group-index 0}
            named-regexes)]
    (-> mp
        (assoc ::multi-pattern (-> mp ::multi-regal regal/regex))
        (dissoc ::last-group-index))))

(defn multi-re-matches
  "Like re-matches (almost) but for a multi-pattern. Returns a map with the name
  of the pattern that matched (under `:pattern-name`), the entire match (under
  `:entire-match`), as well as the groups that would have matched if this regex
  had been used directly with re-matches (under `:groups`)."
  [multi-pattern s]
  (let [{::keys [multi-pattern group-indexes]} multi-pattern]
    (when-let [match (re-matches multi-pattern s)]
      (let [match-start-idx (->> match rest (keep-indexed (fn [i x] (when x i))) first)
            [[begin pattern-name] [end _]] (subseq group-indexes >= match-start-idx)]
        {:pattern-name pattern-name
         :entire-match (first match)
         :groups (subvec match (inc begin) (inc end))}))))
