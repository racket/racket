
------------------------- An R6RS Test Suite -------------------------

======================================================================
Files and libraries
======================================================================

Files that end ".sps" are R6RS programs. The main one is "main.sps",
which runs all the tests.

Files that end ".sls" are R6RS libraries. For example, "base.sls" is a
library that implements `(tests r6rs base)', which is a set of tests
for `(rnrs base)'. Many R6RS implementations will auto-load ".sls"
files if you put the directory of tests in the right place.

In general, for each `(rnrs <id> ... <id>)' in the standard:

 * There's a library of tests "<id>/.../<id>.sls". It defines and
   exports a function `run-<id>-...<id>-tests'.

 * There's a program "run/<id>/.../<id>.sps" that imports
   "<id>/.../<id>.sls", runs the tests, and reports the results.

And then there's "main.sps", which runs all the tests (as noted
above). Also, "test.sls" implements `(tests r6rs test)', which
implements the testing utilities that are used by all the other
libraries.

======================================================================
Limitations and feedback
======================================================================

One goal of this test suite is to avoid using `eval' (except when
specifcally testing `eval'). Avoiding `eval' makes the test suite as
useful as possible to ahead-of-time compilers that implement `eval'
with a separate interpreter. A drawback of the current approach,
however, is that if an R6RS implementation doesn't supply one binding
or does not support a bit of syntax used in a set of tests, then the
whole set of tests fails to load.

A related problem is that each set of sets is placed into one function
that runs all the tests. This format creates a block of code that is
much larger than in a typical program, which might give some compilers
trouble.

In any case, reports of bugs (in the tests) and new tests would be
very much appreciated. File either as a PLT Scheme bug report at

   http://bugs.plt-scheme.org

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

As of Ikarus 0.3.0+ (revision 1548), many libraries fail to load,
mostly because condition names like &error cannot be used as
expressions.

Larceny
-------

Put this directory at "<somewhere>/tests/r6rs" and run with "run.sps"

  larceny -path <somewhere> -r6rs -program run.sps

or run an individual library's test, such as "run/program.sps" as

  larceny -path <somewhere> -r6rs -program run/program.sps

As of Larceny 0.962, many test suites (such as "base.sls") take too
long and use too much memory to load on our machine; probably the test
functions are too big.

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

As of PLT Scheme 4.0.2.5, two tests fail. They correspond to
documented non-conformance with R6RS.

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
