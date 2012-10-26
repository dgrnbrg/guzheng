# lein-guzheng

A Leiningen plugin to perform branch coverage analysis on your project.

## Usage

Put `[lein-guzheng "0.4.4"]` into the `:plugins` vector of your
`:user` profile, or if you are on Leiningen 1.x do `lein plugin install
lein-guzheng 0.4.4`).

One way to run guzheng is to explicitly list the namespaces you want coverage analysis
for, then `--`, and then the task to run the code that will exercise your project (usually
`test` or `midje`.

    $ lein guzheng my.first.ns my.second.ns -- test

lein-guzheng now supports wildcards thanks to sleight:

    $ lein guzheng "my.*" -- test

Runs the test task and does branch coverage analysis on my.first.ns and my.second.ns.

Use this to find dead code (by using the run task), untested code (by using
test or midge), or to test coverage of other lein tasks.

## Common Pitfalls

If you're using wildcard matching, make sure to quote `*` for your shell so that it
isn't expanded by the shell. For example, to use guzheng on all namespaces in bash,
you'll need to use double-quotes around the `*`, or else you'll see no output (unless
you have a namespace with the same name as a file in the directory where you ran
the `lein` command.


## License

Copyright © 2012 David Greenberg

Distributed under the Eclipse Public License, the same as Clojure.
