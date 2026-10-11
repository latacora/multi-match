(ns com.latacora.multi-match.prefix-test
  (:require [clojure.test :as t]
            [com.latacora.multi-match.prefix :as p]
            [lambdaisland.regal :as regal])
  (:import (java.util.regex Pattern PatternSyntaxException)))

(defn- binary-strings [max-length]
  (mapcat (fn strings [n]
            (if (zero? n) [""]
                (for [s (strings (dec n)) c ["a" "b"]] (str s c))))
          (range (inc max-length))))

(t/deftest mandatory-prefixes
  (t/are [form expected] (= expected (p/literal-prefix form))
    [:cat "foo" [:capture [:cat "bar" "baz"]] :digit] "foobarbaz"
    [:cat "foo" [:? "bar"] "baz"] "foo"
    [:cat [:alt "foo" "bar"] "baz"] ""
    [:capture [:* "a"]] ""
    [:cat "" [:capture "foo"] "bar"] "foobar"
    [:cat "foo" [:lookahead "bar"] "bar"] "foo"))

(defn- baseline [patterns input]
  (into [] (keep (fn [[name ^Pattern pattern]]
                   (let [m (.matcher pattern input)]
                     (when (.matches m)
                       {:pattern-name name :entire-match (.group m)
                        :groups (mapv #(.group m (int %))
                                      (range 1 (inc (.groupCount m))))}))))
        patterns))

(defn- check-against-java [compiled originals inputs]
  (doseq [s inputs]
    (t/is (= (baseline originals s) (p/all-matches compiled s)) (pr-str s))))

(t/deftest membership-and-captures
  (let [patterns [[:bare #"foo"]
                  [:optional #"(foo)(bar)?"]
                  [:nested #"((foo)(bar))"]
                  [:other #"(foo)(baz)"]
                  [:fallback #"([a-z]*)"]
                  [:unicode #"(😀)(café)"]
                  [:duplicate #"((foo)(bar))"]]
        compiled (p/compile-matcher patterns)]
    (t/is (= [0 1 4] (p/candidate-ids compiled "foo")))
    (t/is (= [0 1 2 4 6] (p/candidate-ids compiled "foobar")))
    (doseq [s ["" "foo" "foobar" "foobaz" "foobarb" "42" "foobar\n" "😀café"]]
      (t/is (= (baseline patterns s) (p/all-matches compiled s)) s))))

(t/deftest preserve-java-flags-and-unsupported-syntax
  (let [patterns [[:scoped #"(?i:foo)(bar)"]
                  [:flags (Pattern/compile "(foo)" Pattern/CASE_INSENSITIVE)]
                  [:literal (Pattern/compile "foo.*" Pattern/LITERAL)]
                  [:backref #"(foo)\1"]
                  [:named #"(?<value>foo)"]
                  [:lookahead #"(?=foo)(foo)"]]]
    (doseq [s ["FOObar" "FOO" "foo.*" "foofoo" "foo" ""]]
      (t/is (= (baseline patterns s)
               (p/all-matches (p/compile-matcher patterns) s)) s))))

(t/deftest mixed-input-types
  (t/is (= [{:pattern-name :string :entire-match "foobar" :groups ["bar"]}
            {:pattern-name :regal :entire-match "foobar" :groups ["foo" "bar"]}]
           (p/all-matches
            (p/compile-matcher [[:string "foo(bar)"]
                                [:regal [:cat [:capture "foo"] [:capture "bar"]]]])
            "foobar")))
  (t/is (= [] (p/all-matches (p/compile-matcher []) "foo"))))

(t/deftest exhaustive-small-inputs
  (let [forms ["a" "ab" [:cat "a" [:? "b"]]
               [:cat [:capture "a"] [:capture [:* "b"]]]
               [:cat [:alt "a" "b"] "a"] [:* "a"]
               [:cat "" [:capture "ab"]] [:? [:capture "ab"]]]
        originals (mapv (fn [i f] [i (regal/regex f)]) (range) forms)
        inputs (binary-strings 5)]
    (doseq [compiled [(p/compile-matcher originals)
                     (p/compile-matcher (mapv vector (range) forms))]
            s inputs]
      (t/is (= (baseline originals s) (p/all-matches compiled s)) s))))

(t/deftest generated-regal-differential-check
  ;; Deterministic expression generation, independently evaluated by Java.
  ;; Quantified/captured branches exercise where prefix extraction must STOP.
  (let [atoms ["" "a" "b" "ab"]
        forms (vec (concat
                    atoms
                    (for [a atoms b atoms
                          form [[:cat a b]
                                [:alt a b]
                                [:cat [:capture a] [:? [:capture b]]]
                                [:cat [:? a] b]
                                [:cat [:* a] b]
                                [:cat [:+ a] b]
                                [:cat [:capture [:alt a b]] "a"]
                                [:cat a [:capture [:* b]]]
                                [:cat [:capture [:cat a b]] [:? "a"]]]]
                      form)))
        originals (mapv (fn [i f] [i (regal/regex f)]) (range) forms)]
    (doseq [[kind sources] [[:regal (mapv vector (range) forms)]
                           [:pattern originals]
                           [:string (mapv (fn [[id ^Pattern re]] [id (.pattern re)])
                                          originals)]]]
      (t/testing (name kind)
        (let [compiled (p/compile-matcher sources)]
          (doseq [s (binary-strings 5)]
            (let [expected (baseline originals s)
                  ids (set (p/candidate-ids compiled s))]
              (t/is (every? ids (map :pattern-name expected))
                    (str "Candidate filter lost a match on " (pr-str s)))
              (t/is (= expected (p/all-matches compiled s)) (pr-str s)))))))))

(t/deftest escaped-literals-and-special-characters
  (let [literals ["a.b" "a*b" "a+b" "a?b" "a|b" "(a)" "[a]" "{a}"
                  "^a$" "a\\b" "\\E" "\n" "\r\n" "\t" "\u0000"
                  "café" "café" "😀" "😀x" "(?i)a"]
        originals (mapv (fn [i s] [i (Pattern/compile (Pattern/quote s))])
                        (range) literals)
        inputs (concat [""] literals (map #(str % "!") literals))]
    (doseq [sources [originals
                    (mapv (fn [[id ^Pattern re]] [id (.pattern re)]) originals)
                    ;; A bare string is regex source in the public API.
                    (mapv (fn [i s] [i [:cat s]]) (range) literals)]]
      (check-against-java (p/compile-matcher sources) originals inputs)))
  (let [sources ["\\x61(b)" "\\u0061(b)" "\\x{1F600}(x)" "\\t(a)"
                 "\\cA(b)" "\\077(a)" "a\\b" "a\\z" "a\\R(b)"]
        originals (mapv (fn [i s] [i (Pattern/compile s)]) (range) sources)]
    (check-against-java
     (p/compile-matcher originals) originals
     ["ab" "😀x" "\ta" "\u0001b" "?a" "a" "a\nb" "a\r\nb" ""])))

(t/deftest flag-sensitive-matching
  (let [originals [[:comments (Pattern/compile "f o o # comment" Pattern/COMMENTS)]
                   [:canonical (Pattern/compile "é" Pattern/CANON_EQ)]
                   [:unicode (Pattern/compile "é" (bit-or Pattern/CASE_INSENSITIVE
                                                          Pattern/UNICODE_CASE))]
                   [:dotall (Pattern/compile "foo(.*)" Pattern/DOTALL)]
                   [:scoped #"foo(?i:bar)(baz)"]
                   [:inline #"foo(?i)bar"]
                   [:class #"(?U)\w+"]]]
    (check-against-java (p/compile-matcher originals) originals
                        ["foo" "é" "é" "É" "foo\n" "fooBARbaz" "fooBAR" ""])))

(t/deftest order-terminals-and-repeated-calls
  ;; Deliberately put deeper terminals before root/shorter terminals. Trie
  ;; traversal order must not replace caller order or drop duplicate names.
  (let [originals [[:same #"foobar"] [nil #".*"] [:same #"foo.*"]
                   [:empty #""] [false #"foobar"]]
        compiled (p/compile-matcher originals)]
    (t/is (= [0 1 2 3 4] (p/candidate-ids compiled "foobar")))
    (check-against-java compiled originals
                        ["foobar" "" "foo" "foobar" "no" "" "foobar"]))
  (let [originals (array-map :b #"foo.*" :a #"foobar")]
    (check-against-java (p/compile-matcher originals) originals ["foo" "foobar"]))
  (t/is (thrown? PatternSyntaxException
                 (p/compile-matcher [[:invalid "("]]))))
