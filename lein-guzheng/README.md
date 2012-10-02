# lein-guzheng

A Leiningen plugin to run another leiningen plugin with guzheng.

guzheng is an instrumentation library, and this task will do branch
coverage analysis.

[![Build Status](https://secure.travis-ci.org/dgrnbrg/lein-guzheng.png?branch=master)](http://travis-ci.org/dgrnbrg/lein-guzheng)

## Usage

Put `[lein-guzheng "0.4.2"]` into the `:plugins` vector of your
`:user` profile, or if you are on Leiningen 1.x do `lein plugin install
lein-guzheng 0.4.2`.

    $ lein guzheng my.first.ns my.second.ns -- test

Runs the test task and does branch coverage analysis on my.first.ns and my.second.ns.

Use this to find dead code (by using the run task), untested code (by using
test or midge), or to test coverage of other lein tasks.

## License

Copyright © 2012 David Greenberg

Distributed under the Eclipse Public License, the same as Clojure.
