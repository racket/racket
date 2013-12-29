#lang scribble/doc
@(require scribble/manual 
          scribble/bnf
          "common.rkt" 
          (for-label racket/runtime-path
                     launcher/launcher))

@title[#:tag "test"]{@exec{raco test}: Run tests}

The @exec{raco test} command requires and (by default) runs the
@racket[test] submodule (if any) associated with each path given on
the command line. By default, each test is run in a separate Racket
process. Command-line flag can control which submodule is run, whether
to run the main module if no submodule is found, and whether to run
tests as processes or places.

When an argument path refers to a directory, the tool recursively
discovers all files that end in @filepath{.rkt} within the directory
and runs them.

A test is counted as failing if it causes Racket to exit with a
non-zero exit code or (when @Flag{e} or @DFlag{check-stderr} is
specified) if it produces output on the error port.  The current
directory is set to a test file's directory before running the file.

The @exec{raco test} command accepts several flags:

@itemize[

 @item{@Flag{c} or @DFlag{collection}
       --- Intreprets the arguments as collections where whose files should be tested.}

 @item{@Flag{p} or @DFlag{package}
       --- Intreprets the arguments as packages whose files should
       be tested. (All package scopes are searched for the first, most
       specific package.)}

 @item{@DFlag{drdr}
       --- Configures defaults to imitate the DrDr continuous testing
       system: use as many jobs as available processors, set the
       default timeout to 600 seconds, count stderr output as a test failure,
       quiet program output, and print a table of results.}

 @item{@Flag{s} @nonterm{name} or @DFlag{submodule} @nonterm{name}
       --- Requires the submodule @nonterm{name} rather than @racket[test].
       Supply @Flag{s} or @DFlag{submodule} to run multiple submodules,
       or combine multiple submodules with @DFlag{first-avail} to
       run the first available of the listed modules.}

 @item{@Flag{r} or @DFlag{run-if-absent}
       --- Requires the top-level module of a file if a relevant submodule is not 
       present. This is the default mode.}

 @item{@Flag{x} or @DFlag{no-run-if-absent}
       --- Ignores a file if the relevant submodule is not present.}

 @item{@DFlag{first-avail}
       --- When multiple submodule names are provided with @Flag{s} or
       @DFlag{submodule}, runs only the first available submodule.}

 @item{@DFlag{direct}
      --- Runs each test in a thread. This mode is the default if
      a single file is specified. Multiple tests can interfere with
      each other and the overall test run by exiting, unsafe operations
      that block (and thus prevent timeout), and so on.}

 @item{@DFlag{process}
      --- Runs each test in a separate operating-system process. This
          mode is the default if multiple files are specified or if a
          directory, collection, or package is specified.}

 @item{@DFlag{place}
      --- Runs each test in a @tech[#:doc '(lib
      "scribblings/reference/reference.scrbl")]{place}, instead of in an
      operating-system process.}

 @item{@Flag{j} @nonterm{n} or @DFlag{jobs} @nonterm{n}
      --- Runs up to @nonterm{n} tests in parallel.}

 @item{@DFlag{timeout} @nonterm{seconds}
      --- Sets the default timeout (after which a test counts as failed)
      to @nonterm{seconds}. Use @exec{+inf.0} to allow tests to run without
      limit but allow @racket[timeout] sub-submodule configuration.}

 @item{@Flag{Q} or @DFlag{quiet-program}
       --- suppresses output from each test program.}

 @item{@Flag{e} or @DFlag{check-stderr}
       --- count any stderr output as a test failure.}

 @item{@Flag{q} or @DFlag{quiet}
       --- suppresses output of progress information.}

 @item{@DFlag{table} or @Flag{t}
       --- Print a summary table after all tests. If a test uses
       @racketmodname[rackunit], or if a test at least uses
       @racketmodname[rackunit/log] to log successes and failures,
       the table reports test and failure counts based
       on the log.}

]

When @exec{raco test} runs a test in a submodule, a @racket[config]
sub-submodule can provide additional configuration for running the
test. The @racket[config] sub-submodule should use the
@racketmodname[info] module language to define the following
identifiers:

@itemlist[

 @item{@racket[timeout] --- override the default timeout for the test,
       when timeouts are enabled.}

]

To prevent @exec{raco test} from running a particular file, normally
the file should contain a submodule that takes no action. In some
cases, however, adding a submodule is inconvenient or impossible
(e.g., because the file will not always compile). Thus, @exec{raco
test} also consults any @filepath{info.rkt} file in the candidate test
file's directory; in the case of a file within a collection,
@filepath{info.rkt} files from any enclosing collection directories
are also consulted. The following @filepath{info.rkt} fields are
recognized:

@itemlist[

 @item{@racket[test-omit-paths] --- a list of path strings (relative
 to the enclosing directory) or @racket['all] to omit all files within
 the enclosing directory.  When a path string refers to a directory,
 all files within the directory are omitted.}

]
