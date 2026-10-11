# com.latacora/multi-match

Match against multiple regexes simultaneously, and then tell you which one
matched, without leaving the familiarity of regexes for a more sophisticated
parser like [instaparse] or ANTLR4.

The original API assumes all your regexes are mutually exclusive, i.e. it will
only return one match. The prefix-dispatch API below returns all matches.

[instaparse]: https://github.com/Engelberg/instaparse

## Installation

Download from https://github.com/latacora/multi-match

## Usage

The tests are probably your best bet for now: https://github.com/latacora/multi-match/blob/main/test/com/latacora/multi_match_test.clj#L51

## Development

To run the regular tests: `clojure -M:test`.

Property tests are opt-in, on a separate classpath, and are not run by the normal
test command or push CI:

```sh
clojure -M:property-test                # 500 randomized trials
clojure -M:property-test 10000 20260921 # explicit trial count and replay seed
```

The property generator builds nested Regal expressions together with matching
witness strings, then tests those witnesses, modified inputs, and random input.
It checks both candidate completeness (no matching pattern gets filtered out)
and exact ordered results/captures against Java for Regal, regex-string, and
Pattern inputs. Failures include a replay seed and a shrunk counterexample.
Repetition and generation size are bounded to keep the Java oracle tractable;
the regular tests separately cover flags, escaped literals, and backreferences.

## All matches with literal-prefix dispatch

The optional `com.latacora.multi-match.prefix` namespace uses Specter to extract
mandatory literal prefixes from Regal forms and build a shared prefix trie. It
returns every matching pattern, even when patterns overlap:

```clojure
(require '[com.latacora.multi-match.prefix :as prefix])

(def matcher
  (prefix/compile-matcher
   [[:word #"([a-z]+)"]
    [:bar #"(foo)(bar)"]
    [:baz #"(foo)(baz)"]]))

(prefix/all-matches matcher "foobar")
;; => [{:pattern-name :word, :entire-match "foobar", :groups ["foobar"]}
;;     {:pattern-name :bar, :entire-match "foobar", :groups ["foo" "bar"]}]
```

Inputs may be a map or an ordered sequence of name/regex pairs; regexes may be
Java Patterns, regex strings, or Regal forms. Results follow input iteration
order. Compile once and reuse the immutable matcher across threads.

Specter's recursive path descends only into `:cat` and `:capture`, treating all
other operators as opaque. Leading string leaves form a mandatory prefix.
`transform` inserts each pattern ID at its prefix's terminal trie node, sharing
common characters with existing paths. Matching collects IDs along the input's
path, including IDs at the root for patterns with no known prefix, and confirms
each candidate using its original Java regex. Captures need no renumbering.

This is candidate filtering, not a rewrite of the combined Java alternation.
It does not share arbitrary regex fragments such as `[a-z]+/`. Patterns with
Java flags or `(?...` constructs bypass prefix extraction because the current
Regal parser does not preserve all flag semantics. Unsupported parsing also
falls back to unfiltered Java matching. All matching retains Java's backtracking
behavior. The original namespace's first-match API is unchanged.

## Hyperscan experiment

Run `clojure -M:hyperscan` to try the optional
[hyperscan-java](https://github.com/gliwka/hyperscan-java) wrapper, which bundles
Vectorscan native binaries through JavaCPP. This is not an FFM binding.

The standalone experiment in `dev/hyperscan_experiment.clj` compiles separately
identified, whole-string patterns into one native database and returns every
matching pattern. It then uses Java regex only on those matches to extract
captures. The example checks overlapping pattern membership, nested and optional
captures, Unicode, empty input, and whole-string boundaries against a Java baseline.
Native databases and per-scanner scratch space are reused and closed explicitly.

This is a feasibility experiment, not a benchmark or a replacement for the public
API. It uses regex strings with syntax shared by both engines; arbitrary Java
patterns, flags, and Regal forms are not translated. The Java capture pass retains
Java's backtracking behavior. The native dependency is confined to the
`:hyperscan` alias.

To benchmark all-matches throughput:

```sh
clojure -M:hyperscan:hyperscan-bench /tmp/multi-match-bench.edn
```

This compares checking every precompiled Java pattern with the native scan plus
Java capture extraction. Both return the same ordered maps and capture strings;
each case verifies equality before timing. Criterium measures batches of inputs
with 3 seconds of warmup per implementation and 10 interleaved samples targeting
200 ms each. Reported times are microseconds per input. Compilation and scratch
allocation are excluded from scan timing and reported separately for the native
path. The JVM heap is fixed at 512 MB.

Cases cover the small example, 10/100/1000 routing patterns with 75% hits and 25%
misses, a long shared-prefix workload, and a case where all 100 patterns match.
These are synthetic workloads, not production performance guarantees. Repeat
the command in fresh JVMs to check stability. This compares all-matches behavior,
not the existing public API's first-match behavior.

## License

Copyright © Latacora, LLC
