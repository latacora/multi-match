(ns hyperscan-experiment
  "Standalone native experiment; not a general Java-regex compatibility layer."
  (:require [clojure.test :as t])
  (:import (com.gliwka.hyperscan.wrapper Database Expression ExpressionFlag Match Scanner)
           (java.util EnumSet List)
           (java.util.regex Pattern)))

(set! *warn-on-reflection* true)

(def patterns
  ;; Use syntax shared by Java and Hyperscan. Native captures are ignored;
  ;; only matching patterns are run again in Java to extract their groups.
  [[:word "([a-z]+)"]
   [:thing "(foo.*)"]
   [:nested "((foo)(bar))"]
   [:optional "(foo)(bar)?"]
   [:unicode "(café|😀)"]
   [:empty "(a*)"]])

(defn compile-database ^Database [named-patterns]
  (Database/compile
   ^List
   (mapv (fn [id [_ source]]
           (Expression. (str "\\A(?:" source ")\\z")
                        (EnumSet/of ExpressionFlag/UTF8
                                    ExpressionFlag/ALLOWEMPTY
                                    ExpressionFlag/SINGLEMATCH)
                        (Integer/valueOf (int id))))
         (range) named-patterns)))

(defn matching-ids [^Scanner scanner ^Database database ^String input]
  ;; Callback order is not a pattern-priority guarantee. Sort IDs to preserve
  ;; the caller's order, and never depend on the wrapper's substring offsets.
  (into (sorted-set)
        (map (fn [^Match match] (.getId (.getMatchedExpression match))))
        (.scan scanner database input)))

(defn captures [name ^Pattern pattern ^String input]
  (let [matcher (.matcher pattern input)]
    (when (.matches matcher)
      {:pattern-name name
       :entire-match (.group matcher)
       :groups (mapv #(.group matcher (int %))
                     (range 1 (inc (.groupCount matcher))))})))

(defn all-matches [scanner database compiled input]
  (into []
        (keep (fn [id]
                (let [[name pattern] (nth compiled id)]
                  (captures name pattern input))))
        (matching-ids scanner database input)))

(t/deftest native-full-matches
  (let [compiled (mapv (fn [[name source]] [name (Pattern/compile source)]) patterns)]
    (with-open [database (compile-database patterns)
                scanner (Scanner.)]
      (.allocScratch scanner database)
      (doseq [[input expected-names]
              [["foobar" [:word :thing :nested :optional]]
               ["foo" [:word :thing :optional]]
               ["café" [:unicode]]
               ["😀" [:unicode]]
               ["" [:empty]]
               ["aaa" [:word :empty]]
               ["42" []]
               ["!foobar" []]
               ["foobar\n" []]]]
        (t/testing (pr-str input)
          (let [ids (matching-ids scanner database input)
                result (all-matches scanner database compiled input)
                baseline (into [] (keep (fn [[name pattern]]
                                         (captures name pattern input))) compiled)]
            (t/is (= expected-names (mapv #(first (nth patterns %)) ids)))
            (t/is (= baseline result)))))
      (t/is (= ["foo" nil]
               (:groups (last (all-matches scanner database compiled "foo"))))))))

(defn -main [& _]
  (println "Native engine:" (Scanner/getVersion))
  (println "Platform:" (System/getProperty "os.name") (System/getProperty "os.arch"))
  (let [{:keys [fail error]} (t/run-tests 'hyperscan-experiment)]
    (when (pos? (+ fail error))
      (throw (ex-info "Hyperscan experiment failed" {:fail fail :error error}))))
  (let [compiled (mapv (fn [[name source]] [name (Pattern/compile source)]) patterns)]
    (with-open [database (compile-database patterns)
                scanner (Scanner.)]
      (.allocScratch scanner database)
      (println "All whole-string matches for foobar:")
      (doseq [match (all-matches scanner database compiled "foobar")]
        (prn match)))))
