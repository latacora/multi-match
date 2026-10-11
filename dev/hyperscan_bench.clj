(ns hyperscan-bench
  "Synthetic all-matches comparison, including captures and result allocation."
  (:require [criterium.core :as c]
            [hyperscan-experiment :as h])
  (:import (com.gliwka.hyperscan.wrapper Scanner)
           (java.util.regex Pattern)))

(defn java-all [compiled input]
  (into [] (keep (fn [[name pattern]] (h/captures name pattern input))) compiled))

(defn routing-case [n late?]
  (let [prefix (if late? (str (apply str (repeat 256 "a")) "/") "")]
    {:label (str n (if late? "-shared-prefix" "-selective"))
     :patterns (mapv (fn [i]
                       [i (str (when late? "[a-z]+/")
                               (format "route-%04d/" i) "([a-z]+)/([0-9]+)")])
                     (range n))
     ;; 75% hits, 25% misses; spread hits across pattern positions.
     :inputs (mapv (fn [i]
                     (str prefix (format "route-%04d/" (if (zero? (mod i 4))
                                                        (+ n i)
                                                        (mod (* i 37) n)))
                          "alice/" (+ 1000 i)))
                   (range 64))}))

(def cases
  (concat
   [{:label "6-original-example"
     :patterns h/patterns
     :inputs ["foobar" "foo" "café" "😀" "" "aaa" "42" "!foobar" "foobar\n"]}]
   (map #(routing-case % false) [10 100 1000])
   [(routing-case 1000 true)
    {:label "100-all-match"
     :patterns (mapv (fn [i] [i (str "(foo[a-z]+)([0-9]+)(?:x{0," (inc i) "})")])
                     (range 100))
     :inputs (mapv #(str "foobar" (+ 1000 %)) (range 64))}]))

(defn run-case [{:keys [label patterns inputs]}]
  (let [compiled (mapv (fn [[name source]] [name (Pattern/compile source)]) patterns)
        started (System/nanoTime)]
    (with-open [database (h/compile-database patterns)
                scanner (Scanner.)]
      (.allocScratch scanner database)
      (let [compile-ms (/ (- (System/nanoTime) started) 1e6)
            java-f #(mapv (partial java-all compiled) inputs)
            native-f #(mapv (partial h/all-matches scanner database compiled) inputs)]
        (when-not (= (java-f) (native-f))
          (throw (ex-info "Engines disagree" {:case label})))
        (println "Measuring" label "(results verified)")
        (flush)
        (let [[java native]
              (c/benchmark-round-robin*
               [{:f java-f :expr-string "java-all"}
                {:f native-f :expr-string "native-plus-java-captures"}]
               {:warmup-jit-period 3000000000
                :target-execution-time 200000000
                :samples 10 :bootstrap-size 1000})
              us-per-input #(/ (* 1e6 (first (:mean %))) (count inputs))
              result {:case label :patterns (count patterns)
                      :input-count (count inputs)
                      :native-compile-and-scratch-ms compile-ms
                      :java-us (us-per-input java)
                      :native-us (us-per-input native)
                      :speedup (/ (us-per-input java) (us-per-input native))}]
          (prn result)
          (flush)
          ;; Retain raw Criterium measurements for inspection.
          (assoc result :java-stats (dissoc java :results)
                        :native-stats (dissoc native :results)))))))

(defn -main [& [output]]
  (println "JVM:" (System/getProperty "java.vm.name") (System/getProperty "java.version"))
  (println "Native:" (Scanner/getVersion) "Architecture:" (System/getProperty "os.arch"))
  (let [results (mapv run-case cases)]
    (when output (spit output (pr-str results)))))
