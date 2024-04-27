(ns com.latacora.multi-match-test
  (:require
   [com.latacora.multi-match :as mm]
   [clojure.test :as t]))

(t/deftest ->regal-form-tests
  (t/are [input regalified] (= regalified (#'mm/->regal-form input))
    "abc"
    "abc"

    "abc(x)"
    [:cat "abc" [:capture "x"]]

    "abc(x|y|z)"
    [:cat "abc" [:capture [:alt "x" "y" "z"]]]

    "abc(x|y|z)(p)"
    [:cat "abc"
     [:capture [:alt "x" "y" "z"]]
     [:capture "p"]]

    "abc(x|y|z(p)(q)(r))"
    [:cat "abc"
     [:capture
      [:alt "x" "y"
       [:cat "z" [:capture "p"] [:capture "q"] [:capture "r"]]]]]))

(t/deftest count-captures-tests
  (t/are [re n] (= n (-> re (#'mm/->regal-form) (#'mm/count-captures)))
    #"abc" 0
    #"abc(x)" 1
    #"abc(x|y|z)" 1
    #"abc(x|y|z)(p)" 2
    #"abc(x|y|z(p)(q)(r))" 4))

(defn test-multi-pattern
  "Compiles and runs the given multi-pattern, then asserts that it behaves
  analogously to a regular regex pattern match."
  [source test-string expected-result]
  (let [multi-pattern (#'mm/compile-multi-pattern source)
        result (#'mm/multi-re-matches multi-pattern test-string)]

    (t/is (= expected-result result))

    (let [{:keys [entire-match groups pattern-name]} result
          matched-pattern (source pattern-name)]
      (t/is
       (= (into [entire-match] groups)
          (re-matches matched-pattern test-string))))))

(t/deftest multi-pattern-tests
  (t/are [test-string expected-result]
         (test-multi-pattern
          {:as #"(a*)" :bs #"(b*)" :cs #"(c*)"}
          test-string expected-result)

    "aa"
    {:pattern-name :as :entire-match "aa" :groups ["aa"]}

    "aaa"
    {:pattern-name :as :entire-match "aaa" :groups ["aaa"]}

    "bbb"
    {:pattern-name :bs :entire-match "bbb" :groups ["bbb"]})

  (t/are [test-string expected-result]
         (test-multi-pattern
          {:as #"((x)(y)a*)d" :bs #"((p)(q)b*)e" :cs #"((m)(n)c*)f"}
          test-string expected-result)

    "xyaad"
    {:pattern-name :as :entire-match "xyaad" :groups ["xyaa" "x" "y"]}

    "pqbbbbe"
    {:pattern-name :bs :entire-match "pqbbbbe" :groups ["pqbbbb" "p" "q"]}))
