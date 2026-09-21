# Hyperscan experiment results

Measured on 2026-09-19, macOS ARM64, Oracle GraalVM Java 25.0.4, Vectorscan
5.4.11 through hyperscan-java 5.4.11-3.1.0 (JavaCPP). Two fresh JVM runs,
512 MB fixed heap. Reproduce with:

```sh
clojure -M:hyperscan:hyperscan-bench /tmp/multi-match-bench.edn
```

Both implementations return **all** matching patterns, ordered by input pattern
order, with identical capture strings and result maps. Equality is checked before
each case. Java loops over separately precompiled patterns and creates a Matcher
per pattern/input. Native uses the current experiment's String scan API, sorts
matching IDs, then runs Java matchers on those IDs to extract captures.

Criterium: 3-second warmup per implementation per case, 10 interleaved samples
targeting 200 ms each, batches of 64 strings (9 for the original example).
Compilation, scratch allocation, and input generation are outside scan timing.
Means below are normalized to microseconds per input; ranges span the two JVM
runs, not confidence intervals.

| Case | Java µs/input | Native + Java captures µs/input | Outcome |
| --- | ---: | ---: | --- |
| 6 original example patterns | 0.267–0.281 | 0.946–0.956 | Java 3.4–3.6× faster |
| 10 routing patterns | 0.350–0.363 | 0.798–0.805 | Java 2.2–2.3× faster |
| 100 routing patterns | 2.47–2.78 | 0.929–0.944 | Native 2.6–3.0× faster |
| 1000 routing patterns | 30.4–32.5 | 0.958–0.979 | Native 31–34× faster |
| 1000 patterns, 256-character shared prefix | 1053–1068 | 2.38–2.46 | Native 434–442× faster |
| 100 patterns, every pattern matches | 10.7–10.8 | 44.4–48.7 | Java 4.1–4.5× faster |

Routing inputs have 75% single-pattern hits and 25% misses. The shared-prefix
case intentionally exposes redundant scanning by the Java loop; this large gain
is not representative of arbitrary regex workloads. The all-match case exposes
native result-reporting/sorting overhead when no Java matching work is avoided.

Native compile + scratch setup was about 45 ms for 100 routing patterns,
0.71–0.74 seconds for 1000 routing patterns, and 0.77–0.79 seconds for the
shared-prefix set. Databases need reuse to amortize that cost.

These are synthetic comparisons of the current prototype against a straightforward
all-pattern Java loop, not a claim about the fastest possible Java implementation
or the existing first-match API. Prefix dispatch, matcher reuse, different input
lengths, native byte-buffer/callback APIs, and different match density could change
the outcome. No native-only capture extraction is being measured.
