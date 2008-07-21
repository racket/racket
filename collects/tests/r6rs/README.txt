
R6RS Test Suite

======================================================================
Files
======================================================================

Files that end ".sps" are R6RS programs. The main one is "main.sps",
which runs all the tests.

Files that end ".sls" are R6RS libraries. For example, "base.sls" is a
library that implements tests for `(r6rs base)'. Many R6RS
implementations will auto-load ".sls" files if you set up your
directoties right.

In general, for each `(r6rs <id> ... <id>)' in the standard:

 * There's a library of tests "<id>/.../<id>.sls". It defines and
   exports a function `run-<id>-...<id>-tests'.

 * There's a program "run/<id>/.../<id>.sps" that runs just the
   library's tests and reports the results.

======================================================================
Hints on running the tests
======================================================================

Ikarus
------

Put this directory at "<somewhere>/tests/r6rs" and run with "run.sps"

  cd <somewhere>
  ikarus --r6rs-script tests/r6rs/run.sps

or run an individual library's test, such as "run/program.sps" as

  cd <somewhere>
  ikarus --r6rs-script tests/r6rs/run/program.sps

Larceny
-------

Put this directory at "<somewhere>/tests/r6rs" and run with "run.sps"

  larceny -path <somewhere> -r6rs -program run.sps

or run an individual library's test, such as "run/program.sps" as

  larceny -path <somewhere> -r6rs -program run/program.sps

PLT Scheme
----------

If you get an SVN-based or the "Full" nightly build, then these tests are
in a `tests/r6rs' collection already. You can run all of the tests using

   mzscheme -l tests/r6rs/run.sps

and so on.

Otherwise, install this directory as a `tests/r6rs' collection,
perhaps in the location reported by

  (build-path (find-system-path 'addon-dir) 
              (version) "collects"
              "tests" "r6rs")

You could also play with the PLTCOLLECTS environment variable.

Ypsilon
-------

[If there's a library-autoload mechanism, we didn't figure it
 out. Better ideas are welcome...]

Load the library declarations that you're interested in. For `(rnrs
<id> ... <id>)':

   * Load "test.sls"
   * Load "<id>/...<id>.sls"
   * Eval `(import tests r6rs <id> ... <id>)'
   * Eval `(run-<id>-...<id>-tests)'
   * Eval `(import tests r6rs test)'
   * Eval `(show-test-results)'

======================================================================
Feedback
======================================================================

Reports of bugs (in the tests) and new tests would be very much
appreciated. File either as a PLT Scheme bug report at

   http://bugs.plt-scheme.org
