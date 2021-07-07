# Interval maps and sets

[![Clojars](https://img.shields.io/clojars/v/io.helins/interval.svg)](https://clojars.org/io.helins/interval)

[![Cljdoc](https://cljdoc.org/badge/io.helins/interval)](https://cljdoc.org/d/io.helins/interval)

![CircleCI](https://circleci.com/gh/helins/interval.cljc.svg?style=shield)

Compatible with Clojurescript.

Storing intervals in sorted maps and sets. Commonly known as
[interval trees](https://en.wikipedia.org/wiki/Interval_tree).

Opens many data science applications, from geometry to genomic analysis.

Advantages over common implementations and other languages:

- First class in Clojure, leverages sorted maps and sets
- Hence, persistent and immutable
- Open, adding/removing intervals do not recompute the whole tree
- Complex and convenient querying, see music example below


## Usage

This is an overview.

The [full API is available in Cljdoc](https://cljdoc.org/d/io.helins/interval).

First, we shall explore interval maps as they are more interesting. They map
intervals to sets of values, such as intervals of seconds to one or several
musical notes.

Then, we shall quickly see similar concepts for interval sets. They record
intervals in an "on/off" fashion, without values, such as keeping track of when
was a temperature above a threshold.

Requiring namespaces:

```clojure
(require '[helins.interval.map :as imap]
         '[helins.interval.set :as iset])
```

### Glossary

Following terms are used in the examples:

| Name | Description |
|---|---|
| Interval | A tuple `[Point Point]` designating an interval (**start inclusive**, **end exclusive**) |
| Point | An arbitrary numerical value (seconds, centimeters, inches, ...) or nil for infinity |
| Segment | A tuple `[Interval ValueSet]` |
| ValueSet | A set of arbitrary values |


### Defining an interval map

Following examples are available in the [helins.interval.example.music](../main/src/example/helins/interval/example/music.cljc)
namespace to test at the REPL.

Let us define our short piece of music by starting from an empty interval map
and then "marking" intervals with musical notes. Assuming intervals are
expressed in seconds:

```clojure
(def music
     (-> imap/empty
         ;; Broken C minor chord.
         (imap/mark  0  8 :c)
         (imap/mark  3  8 :e-flat)
         (imap/mark  5  8 :g)
         ;; After a pause, G is repeated.
         (imap/mark 10 11 :g)))
```

### Querying a point an interval map

A precise point is queried using the standard `get` function which returns a
**ValueSet**.

```clojure
;; What notes are played at second 4?
;;
(= (get music
        4)

   #{:c
     :e-flat})


;; Nothing is being played at second 9.
;;
(nil? (get music
           9))
```

Standard `find` returns the interval as well:

```clojure
(= (find music
         4)
   
   [[3 5] #{:c
            :e-flat}])
```

### Querying segments in an interval map

Converting a map to a sequence shows all **Segments**.

We can see exactly how notes are played, when they start, end, how they overlap:

```clojure
(= (seq music)

   (list [[0 3]   #{:c}]
         [[3 5]   #{:c
                    :e-flat}]
         [[5 8]   #{:c
                    :e-flat
                    :g}]
         [[10 11] #{:g}]))
```

It is often useful to query **Segments** for parts of an interval map. This is done
by using standard `subseq` since an interval map is a Clojure sorted map.

```clojure
;; What was happening in-between second 2 and second 4?
;;
;; Are returned only relevant segments.
;;
(= (subseq music
           >= 2
           <= 4)

   (list [[0 3] #{:c}]
         [[3 5] #{:c
                  :e-flat}]))
```

### Working with segments

Segments are easy to work with and can offer many insights. For instance:

```clojure
;; Reconstructs exactly what notes are played at what intervals.
;;
;; G has two intervals. Indeed, it is played during the broken chord and
;; on its own a bit later, so:
;;
(= (imap/by-value (seq music))
  
   {:c      #{[ 0  8]}
    :e-flat #{[ 3  8]}
    :g      #{[ 5  8]
              [10 11]}})


;; What notes are being played, in total?
;;
(= (imap/union (seq music))
   
   #{:c
     :e-flat
     :g})
```

### Erasing intervals in interval maps

Values can be selectively erased.

For instance, our C note is being played throughout the whole broken chord
(from second 0 to second 8). We could start it later by erasing its beginning:

```clojure
(def music-2
     (imap/erase music
                 0
                 4
                 :c))


(nil? (get music-2
           1))


(= (get music-2
        4)

   #{:c
     :e-flat})

```

Erasing is very flexible and permissive:

```clojure
;; Nothing changes, :whatever is not even recorded.
;;
(= music
   (imap/erase music
               0
               56456
               :whatever))
```

### Working with interval sets

Following examples are available in the [helins.interval.example.temperature](../main/src/example/helins/interval/example/temperature.cljc)
namespace.

Interval sets are like interval maps but without values. They are a lot simpler
and are meant to record a single phenomenon.

Supposing we recorded temperature above a certain threshold, for a day:

```clojure
(def too-hot
     (-> iset/empty
         (iset/mark 11 13)
         (iset/mark 14 16)
         (iset/erase 15 20)))


;; Was it too hot at noon?
;; Yes, an interval proofs that.
;;
(= (get too-hot
        12)
   [11 13])


;; Was it too hot at hour 16?
;; No interval found, hence no.
;;
(nil? (get too-hot
           16))


;; Segments:
;;
(= (seq too-hot)

   (list [11 13]
         [14 15]))


;; What happened before noon?
;;
(= (subseq too-hot
           >= 0
           <= 12)
   (list [11 13]))
```


### Open intervals and infinity

Infinity is designated by nil, meaning an absence of boundary:

```clojure
;; Interval from 5 to +Infinity
;;
[5 nil]


;; Interval from -Infinity to 5
;;
[nil 5]



;; Infinity
;;
[nil nil]
```

Hence, handling infinity happens naturally:

```clojure
;; An interval set with an interval from 5 to +Infinity.
;;
(def infinite-from-5
     (iset/mark iset/empty
                5
                nil))


;; Is there something at this very high point?
;; Yes, a segment towards infinity proves that.
;;
(= (get infinite-from-5
        78456544)

   [5 nil])
```

## Running tests

Clojure, on the JVM, using [Kaocha](https://github.com/lambdaisland/kaocha):

```bash
$ ./bin/test/jvm/run
```
Clojurescript, on NodeJS, using [Shadow-CLJS](https://github.com/thheller/shadow-cljs):

```bash
$ ./bin/test/node/run

# Or testing an advanced build:
$ ./bin/test/node/advanced
```


## Development

Starting in Clojure JVM mode, mentioning an additional Deps alias (here, a local
setup of NREPL):
```bash
$ ./bin/dev/clojure :nrepl
```

Starting in CLJS mode using [Shadow-CLJS](https://github.com/thheller/shadow-cljs):
```bash
$ ./bin/dev/cljs
# Then open ./cljs/index.html
```


## Credits

These implementations are based on [this original idea](http://clj-me.cgrand.net/2012/03/16/a-poor-mans-interval-tree/).
It took a lot of work overcoming fundamental issues (especially fragmentation)
but it served as an important spark leading to upgrading existing sorted collections in
a creative way.


## License

Copyright © 2020 Adam Helinski

Licensed under the term of the Mozilla Public License 2.0, see LICENSE.
