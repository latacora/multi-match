(ns prefix-properties
  "Opt-in differential property tests; not on the normal test classpath."
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.latacora.multi-match.prefix :as prefix]
            [lambdaisland.regal :as regal]))

(def literal-gen
  (gen/elements ["" "a" "b" "ab" "foo" "." "*" "?" "(" "[" "\\"
                 "\\E" "\n" "\t" "\u0000" "é" "é" "😀"]))

(def expression-gen
  ;; Carry a matching witness so tests exercise successful matches and captures,
  ;; rather than mostly proving that two engines reject random strings.
  (gen/recursive-gen
   (fn [inner]
     (gen/one-of
      [(gen/fmap (fn [{:keys [form witness]}]
                   {:form [:capture form] :witness witness}) inner)
       (gen/fmap (fn [[a b]]
                   {:form [:cat (:form a) (:form b)]
                    :witness (str (:witness a) (:witness b))})
                 (gen/tuple inner inner))
       (gen/fmap (fn [[a b choose-a?]]
                   {:form [:alt (:form a) (:form b)]
                    :witness (:witness (if choose-a? a b))})
                 (gen/tuple inner inner gen/boolean))
       (gen/fmap (fn [[a present?]]
                   {:form [:? (:form a)] :witness (if present? (:witness a) "")})
                 (gen/tuple inner gen/boolean))
       ;; Bound repeats to keep the Java oracle safe even for nullable children.
       (gen/fmap (fn [[a copies]]
                   {:form [:repeat (:form a) 0 2]
                    :witness (apply str (repeat copies (:witness a)))})
                 (gen/tuple inner (gen/choose 0 2)))]))
   (gen/fmap (fn [s] {:form [:cat s] :witness s}) literal-gen)))

(defn- expected-match [id pattern input]
  ;; Use Clojure's existing regex API as the oracle, independent of our matcher
  ;; allocation/capture extraction code.
  (when-let [match (re-matches pattern input)]
    (let [groups (if (string? match) [match] match)]
      {:pattern-name id :entire-match (first groups) :groups (vec (rest groups))})))

(def all-matches-agree
  (prop/for-all [expressions (gen/vector expression-gen 0 8)
                noise (gen/vector literal-gen 0 5)]
    (let [forms (mapv :form expressions)
          patterns (mapv regal/regex forms)
          witnesses (mapv :witness expressions)
          inputs (distinct (concat ["" (apply str noise)] witnesses
                                   (map #(str % "!") witnesses)))
          expected (mapv (fn [input]
                           (into [] (keep-indexed #(expected-match %1 %2 input))
                                 patterns)) inputs)]
      (and
       ;; Sanity-check the generator as well: witnesses must actually match.
       (every? true? (map #(boolean (re-matches %1 %2)) patterns witnesses))
       (every?
        (fn [sources]
          (let [compiled (prefix/compile-matcher (mapv vector (range) sources))]
            (every?
             true?
             (map (fn [input matches]
                    (let [candidates (set (prefix/candidate-ids compiled input))]
                      (and (every? candidates (map :pattern-name matches))
                           (= matches (prefix/all-matches compiled input)))))
                  inputs expected))))
        [forms patterns (mapv str patterns)])))))

(defn -main [& [trials seed]]
  (let [trials (if trials (Long/parseLong trials) 500)
        options (cond-> [:max-size 12]
                  seed (conj :seed (Long/parseLong seed)))
        result (apply tc/quick-check trials all-matches-agree options)]
    (prn result)
    (when-not (:pass? result)
      (throw (ex-info "Prefix property failed; see seed and shrunk counterexample"
                      result)))))
