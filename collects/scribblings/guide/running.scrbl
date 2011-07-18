#lang scribble/doc
@(require scribble/manual "guide-utils.rkt" (for-syntax racket/pretty))

@title[#:tag "running" #:style 'toc]{Running and Creating Executables}

While developing programs, many Racket programmers use the
@seclink[#:doc '(lib "scribblings/drracket/drracket.scrbl")
"top"]{DrRacket} programming environment. To run a program without the
development environment, use @exec{racket} (for console-based
programs) or @exec{gracket} (for GUI programs). This chapter mainly
explains how to run @exec{racket} and @exec{gracket}.

@local-table-of-contents[]

@; ----------------------------------------------------------------------

@section[#:tag "racket"]{Running @exec{racket} and @exec{gracket}}

The @exec{gracket} executable is the same as @exec{racket}, but with
small adjustments to behave as a GUI application rather than a console
application. For example, @exec{gracket} by default runs in
interactive mode with a GUI window instead of a console prompt. GUI
applications can be run with plain @exec{racket}, however.

Depending on command-line arguments, @exec{racket} or @exec{gracket}
runs in @seclink["start-interactive-mode"]{interactive mode},
@seclink["start-module-mode"]{module mode}, or
@seclink["start-load-mode"]{load mode}.

@subsection[#:tag "start-interactive-mode"]{Interactive Mode}

When @exec{racket} is run with no command-line arguments (other than
confguration options, like @Flag{j}), then it starts a @tech{REPL}
with a @litchar{> } prompt:

@verbatim[#:indent 2]{
  @(regexp-replace #rx"\n+$" (banner) "")
  > 
}

@margin-note{For enhancing your @tech{REPL} experience, see
  @racketmodname[xrepl]; for information on GNU Readline support, see
  @racketmodname[readline].}

To initialize the @tech{REPL}'s environment, @exec{racket} first
requires the @racketmodname[racket/init] module, which provides all of
@racket[racket], and also installs @racket[pretty-print] for display
results. Finally, @exec{racket} loads the file reported by
@racket[(find-system-path 'init-file)], if it exists, before starting
the @tech{REPL}.

If any command-line arguments are provided (other than configuration
options), add @Flag{i} or @DFlag{repl} to re-enable the
@tech{REPL}. For example,

@commandline{racket -e '(display "hi\n")' -i}

displays ``hi'' on start-up, but still presents a @tech{REPL}.

If module-requiring flags appear before @Flag{i}/@DFlag{repl}, they
cancel the automatic requiring of @racketmodname[racket/init]. This
behavior can be used to initialize the @tech{REPL}'s environment with
a different language. For example,

@commandline{racket -l racket/base -i}

starts a @tech{REPL} using a much smaller initial language (that loads
much faster). Beware that most modules do not provide the basic syntax
of Racket, including function-call syntax and @racket[require]. For
example,

@commandline{racket -l racket/date -i}

produces a @tech{REPL} that fails for every expression, because
@racketmodname[racket/date] provides only a few functions, and not the
@racket[#%top-interaction] and @racket[#%app] bindings that are needed
to evaluate top-level function calls in the @tech{REPL}.

If a module-requiring flag appears after @Flag{i}/@DFlag{repl} instead
of before it, then the module is required after
@racketmodname[racket/init] to augment the initial environment. For
example,

@commandline{racket -i -l racket/date}

starts a useful @tech{REPL} with @racketmodname[racket/date] available
in addition to the exports of @racketmodname[racket].

@; ----------------------------------------

@subsection[#:tag "start-module-mode"]{Module Mode}

If a file argument is supplied to @exec{racket} before any
command-line switch (other than configuration options), then the file
is required as a module, and (unless @Flag{i}/@DFlag{repl} is
specified), no @tech{REPL} is started. For example,

@commandline{racket hello.rkt}

requires the @filepath{hello.rkt} module and then exits. Any argument
after the file name, flag or otherwise, is preserved as a command-line
argument for use by the required module via
@racket[current-command-line-arguments].

If command-line flags are used, then the @Flag{u} or
@DFlag{require-script} flag can be used to explicitly require a file
as a module.  The @Flag{t} or @DFlag{require} flag is similar, except
that additional command-line flags are processed by @exec{racket},
instead of preserved for the required module. For example,

@commandline{racket -t hello.rkt -t goodbye.rkt}

requires the @filepath{hello.rkt} module, then requires the
@filepath{goodbye.rkt} module, and then exits.

The @Flag{l} or @DFlag{lib} flag is similar to
@Flag{t}/@DFlag{require}, but it requires a module using a
@racket[lib] module path instead of a file path. For example,

@commandline{racket -l raco}

is the same as running the @exec{raco} executable with no arguments,
since the @racket[raco] module is the executable's main module.

Note that if you wanted to pass command-line flags to
@racket[raco] above, you would need to protect the flags with a
@Flag{-}, so that @exec{racket} doesn't try to parse them itself:

@commandline{racket -l raco -- --help}

@; ----------------------------------------

@subsection[#:tag "start-load-mode"]{Load Mode}

The @Flag{f} or @DFlag{load} flag supports @racket[load]ing top-level
expressions in a file directly, as opposed to expressions within a
module file. This evaluation is like starting a @tech{REPL} and typing
the expressions directly, except that the results are not printed.
For example,

@commandline{racket -f hi.rkts}

@racket[load]s @filepath{hi.rkts} and exits. Note that load mode is
generally a bad idea, for the reasons explained in
@secref["use-module"]; using module mode is typically better.

The @Flag{e} or @DFlag{eval} flag accepts an expression to evaluate
directly. Unlike file loading, the result of the expression is
printed, as in a @tech{REPL}. For example,

@commandline{racket -e '(current-seconds)'}

prints the number of seconds since January 1, 1970.

For file loading and expression evaluation, the top-level environment
is created in the same way for
@seclink["start-interactive-mode"]{interactive mode}:
@racketmodname[racket/init] is required unless another module is
specified first. For example,

@commandline{racket -l racket/base -e '(current-seconds)'}

likely runs faster, because it initializes the environment for
evaluation using the smaller @racketmodname[racket/base] language,
instead of @racketmodname[racket/init].

@; ----------------------------------------------------------------------

@include-section["scripts.scrbl"]

@; ----------------------------------------------------------------------

@section[#:tag "exe"]{Creating Stand-Alone Executables}

@(define raco-doc '(lib "scribblings/raco/raco.scrbl"))

For information on creating and distributing executables, see
@secref[#:doc raco-doc "exe"] and @secref[#:doc raco-doc "exe-dist"] in
@other-manual[raco-doc].
