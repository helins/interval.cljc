# Interval

[![Clojars
Project](https://img.shields.io/clojars/v/dvlopt/interval.svg)](https://clojars.org/dvlopt/interval)

[![cljdoc badge](https://cljdoc.org/badge/dvlopt/interval)](https://cljdoc.org/d/dvlopt/interval)

Compatible with Clojurescript.


## Run tests

### Using Kaocha

Run all tests (JVM and JS based ones):

```bash
$ ./bin/kaocha
```

For Clojure only:

```bash
$ ./bin/kaocha jvm
```

For Clojurescript on NodeJS, `ws` must be installed:
```bash
$ npm i ws
```
Then:
```
$ ./bin/kaocha node
```

For Clojurescript in the browser (which might need to be already running):
```bash
$ ./bin/kaocha browser
```

### Using Chui for browser testing

For more convenient and thorough browser testing:

```bash
$ ./bin/chui $COMMAND $ARGS
```

Where `$COMMAND` is `compile`, `watch`, or `release` (for testing a build with
advanced optimizations). When using `release`, providing `--debug` is extemely
useful when something goes wrong (eg. names are not munged in stacktraces).

When ready, open `./chui/index.html` in your favorite browser.


## License

Copyright © 2020 Adam Helinski

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
