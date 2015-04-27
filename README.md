# bolth

![](https://raw.github.com/yeller/bolth/master/doc/bolth.jpg)

> My name is Bolth, Lightning Bolth.

A Clojure library that provides a better test runner for `clojure.test`. Humane
failure output (with pprinted data diffs), humane exception stacktraces, colorized test
results, and a variety of reporting options.

## Installation

grab from clojars

## Usage

There's one main function: `bolth/run-all-tests`.

`run-all-tests` mirrors the interface of `clojure.test/run-all-tests`: it takes
a regex to specify which tests to run. However, it also takes an option map with a whole load
of options (see the docstring for more).

Here's what it looks like in typical usage with a bunch of the options enabled:

![](https://raw.github.com/yeller/bolth/master/doc/normal_run.gif)

![](https://raw.github.com/yeller/bolth/master/doc/failing_run.gif)

![](https://raw.github.com/yeller/bolth/master/doc/slow_test_run.gif)

There's also an helper function that uses `io.aviso.exception/write-exception`
to call clojure.tools.namespace.repl/refresh and pretty print the exception
from that if there is one. See the example `script/test` for sample usage with
grenchman.

## License

Copyright © 2015 Tom Crayford

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

# Creative Commons

Usain Bolth photo used under Creative Commons from https://www.flickr.com/photos/bruneluniversity/3794955418
