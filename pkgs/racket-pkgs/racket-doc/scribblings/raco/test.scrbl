#lang scribble/doc
@(require scribble/manual 
          scribble/bnf
          "common.rkt" 
          (for-label racket/runtime-path
                     racket/base
                     launcher/launcher
                     rackunit/log))

@title[#:tag "test"]{@exec{raco test}: Run tests}

The @exec{raco test} command requires and runs the (by default)
@racket[test] submodule associated with each path given on the command
line. Command-line flag can control which submodule is run, whether to
run the main module if no submodule is found, and whether to run tests
directly, in separate processes (the default), or in separate places.
The current directory is set to a test file's directory before running
the file.

When an argument path refers to a directory, @exec{raco test}
recursively discovers and runs all files within the directory that end
in @filepath{.rkt}, end in @filepath{.scrbl}, or have a (possibly
empty) list of command-line arguments provided by
@racket[test-command-line-arguments] in an @filepath{info.rkt} file.
At the same time, @exec{raco test} omits files and directories within
a directory as directed by @racket[test-omit-paths] in an
@filepath{info.rkt} file.

A test is counted as failing if it logs a failing test code via
@racket[test-log!], causes Racket to exit with a non-zero exit code, or
(when @Flag{e} or @DFlag{check-stderr} is specified) if it produces
output on the error port.

The @exec{raco test} command accepts several flags:

@itemize[

 @item{@Flag{c} or @DFlag{collection}
       --- Interprets the arguments as collections whose content
       should be tested (in the same way as directory content).}

 @item{@Flag{p} or @DFlag{package}
       --- Interprets the arguments as packages whose contents should
       be tested (in the same way as directory content). All package
       scopes are searched for the first, most specific @tech[#:doc
       '(lib "pkg/scribblings/pkg.scrbl")]{package scope}.}
 
 @item{@Flag{l} or @DFlag{lib}
       --- Interprets the arguments as libraries that should be tested.}

 @item{@Flag{m} or @DFlag{modules}
       --- Not only interprets the arguments as paths (which is the
       default mode), but treats them the same as paths found in a
       directory, which means ignoring a file argument that does not
       have the extension @filepath{.rkt}, does not have the extension
       @filepath{.scrbl}, or is not enabled explicitly via
       @racket[test-command-line-arguments] in an @filepath{info.rkt}
       file; meanwhile, paths that are otherwise enabled can be disabled
       via @racket[test-omit-paths] in an @filepath{info.rkt} file.}

 @item{@DFlag{drdr}
       --- Configures defaults to imitate the DrDr continuous testing
       system: ignore non-modules, run tests in separate processes,
       use as many jobs as available processors,
       set the default timeout to 90 seconds, 
       create a fresh @envvar{PLTUSERHOME} and @envvar{TMPDIR} for each test,
       count stderr output as a test failure,
       quiet program output, 
       provide empty program input,
       and print a table of results.}

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
      limit but allow @racket[timeout] sub-submodule configuration.
      If any test fails due to a timeout, the exit status of @exec{raco test}
      is 2 (as opposed to 1 for only non-timeout failures or 0 for success).}

 @item{@DFlag{fresh-user}
      --- When running tests in a separate process, creates a fresh
      directory and sets @envvar{PLTUSERHOME} and @envvar{TMPDIR}. The
      @envvar{PLTADDONDIR} environment variable is also set so that
      the add-on directory (which is where packages are installed, for
      example) does @emph{not} change for each test process.}

 @item{@DFlag{empty-stdin}
       --- Provide an empty stdin to each test program.}

 @item{@Flag{Q} or @DFlag{quiet-program}
       --- Suppresses output from each test program.}

 @item{@Flag{e} or @DFlag{check-stderr}
       --- Count any stderr output as a test failure.}

 @item{@DPFlag{ignore-stderr} @nonterm{pattern}
       --- Don't count stderr output as a test failure if it matches
       @nonterm{pattern}.  This flag can be used multiple times, and
       stderr output is treated as success as long as it matches any
       one @nonterm{pattern}.}

 @item{@Flag{q} or @DFlag{quiet}
       --- Suppresses output of progress information, responsible
       parties, and varying output (see @secref["test-responsible"]).}

 @item{@DFlag{heartbeat}
       --- Periodically report that a test is still running after
       the test has been running at least 5 seconds.}

 @item{@DFlag{table} or @Flag{t}
       --- Print a summary table after all tests. If a test uses
       @racketmodname[rackunit], or if a test at least uses
       @racket[test-log!] from @racketmodname[rackunit/log] to log
       successes and failures, the table reports test and failure
       counts based on the log.}

]

@history[#:changed "1.1" @elem{Added @DFlag{heartbeat}.}]

@section[#:tag "test-config"]{Test Configuration by Submodule}

When @exec{raco test} runs a test in a submodule, a @racket[config]
sub-submodule can provide additional configuration for running the
test. The @racket[config] sub-submodule should use the
@racketmodname[info] module language to define the following
identifiers:

@itemlist[

 @item{@racket[timeout] --- a real number to override the default
       timeout for the test, which applies only when timeouts are
       enabled.}

 @item{@racket[responsible] --- a string, symbol, or list of symbols
       and strings identifying a responsible party that should be
       notified when the test fails. See @secref["test-responsible"].}

 @item{@racket[lock-name] --- a string that names a lock file that is
       used to serialize tests (i.e., tests that have the same lock
       name do not run concurrently). The lock file's location is
       determined by the @envvar{PLTLOCKDIR} enviornment varible or
       defaults to @racket[(find-system-path 'temp-dir)]. The maximum
       time to wait on the lock file is determined by the
       @envvar{PLTLOCKTIME} environment variable or defaults to 4
       hours.}

 @item{@racket[random?] --- if true, indicates that the test's output
       is expected to vary. See @secref["test-responsible"].}

]


@section[#:tag "test-config-info"]{Test Configuration by @filepath{info.rkt}}

Submodule-based test configuration is preferred (see
@secref["test-config"]). In particular, to prevent @exec{raco test}
from running a particular file, normally the file should contain a
submodule that takes no action.

In some cases, however, adding a submodule is inconvenient or
impossible (e.g., because the file will not always compile). Thus,
@exec{raco test} also consults any @filepath{info.rkt} file in the
candidate test file's directory. In the case of a file within a
collection, @filepath{info.rkt} files from any enclosing collection
directories are also consulted for @racket[test-omit-paths]. Finally,
for a file within a package, the package's @filepath{info.rkt} is
consulted for @racket[pkg-authors] to set the default responsible
parties (see @secref["test-responsible"]) for all files in the
package.

The following @filepath{info.rkt} fields are recognized:

@itemlist[

 @item{@racket[test-omit-paths] --- a list of path strings (relative
       to the enclosing directory) or @racket['all] to omit all files
       within the enclosing directory.  When a path string refers to a
       directory, all files within the directory are omitted.}

 @item{@racket[test-command-line-arguments] --- a list of
       @racket[(list _module-path-string (list _argument-path-string
       ...))], where @racket[current-command-line-arguments] is set to
       a vector that contains the @racket[_argument-path-string] when
       running @racket[_module-path-string].}

 @item{@racket[test-timeouts] --- a list of @racket[(list
       _module-path-string _real-number)] to override the default
       timeout for @racket[_module-path-string].}

 @item{@racket[test-responsibles] --- a list of @racket[(list
       _module-path-string _party)] or @racket[(list 'all _party)] to
       override the default responsible party for
       @racket[_module-path-string] or all files within the directory
       (except as overridden), respectively. Each @racket[_party] is a
       string, symbol, or list of symbols and strings. See
       @secref["test-responsible"].}

 @item{@racket[test-lock-names] --- a list of @racket[(list
       _module-path-string _lock-string)] to declare a lock file name
       for @racket[_module-path-string]. See @racket[lock-name] in
       @secref["test-config"].}

 @item{@racket[test-randoms] --- a list of path strings (relative to
       the enclosing directory) for modules whose output varies.
       See @secref["test-responsible"].}

]

@section[#:tag "test-responsible"]{Responsible-Party and Varying-Output Logging}

When a test has a declared responsible party, then the test's output
is prefixed with a

@verbatim[#:indent 2]{raco test:@nonterm{which} @"@"(test-responsible '@nonterm{responsible})}

line, where @nonterm{which} is a space followed by an exact
non-negative number indicating a parallel task when parallelism is
enabled (or empty otherwise), and @nonterm{responsible} is a string,
symbol, or list datum.

When a test's output (as written to stdout) is expected to vary across
runs---aside from varying output that has the same form as produced by
@racket[time]---then it should be declared as varying. In that case,
the test's output is prefixed with a

@verbatim[#:indent 2]{raco test:@nonterm{which} @"@"(test-random #t)}

line.


