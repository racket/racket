#lang scribble/doc
@(require scribble/manual
          "guide-utils.ss"
          (for-syntax scheme/pretty))

@title[#:tag "running" #:style 'toc]{Running and Creating Executables}

While developing programs, many PLT Scheme programmers use the
@seclink[#:doc '(lib "scribblings/drscheme/drscheme.scrbl")
"top"]{DrScheme} programming environment. To run a program without the
development environment, use @exec{mzscheme} (for console-based
programs) or @exec{mred} (for GUI program). This chapter mainly
explains how to run @exec{mzscheme} and @exec{mred}.

@local-table-of-contents[]

@; ----------------------------------------------------------------------

@section[#:tag "mzscheme"]{Running @exec{mzscheme} and @exec{mred}}

Depending on command-line arguments, @exec{mzscheme} or @exec{mred}
runs in @seclink["start-interactive-mode"]{interactive mode},
@seclink["start-module-mode"]{module mode}, or
@seclink["start-load-mode"]{load mode}.

@subsection[#:tag "start-interactive-mode"]{Interactive Mode}

When @exec{mzscheme} is run with no command-line arguments (other than
confguration options, like @Flag{j}), then it starts a @tech{REPL}
with a @litchar{> } prompt:

@verbatim[#:indent 2]{
  Welcome to MzScheme
  > 
}

@margin-note{For information on GNU Readline support, see
@schememodname[readline].}

To initialize the @tech{REPL}'s environment, @exec{mzscheme} first
requires the @schememodname[scheme/init] module, which provides all of
@scheme[scheme], and also installs @scheme[pretty-print] for display
results. Finally, @exec{mzscheme} loads the file reported by
@scheme[(find-system-path 'init-file)], if it exists, before starting
the @tech{REPL}.

If any command-line arguments are provided (other than configuration
options), add @Flag{i} or @DFlag{repl} to re-enable the
@tech{REPL}. For example,

@commandline{mzscheme -e '(display "hi\n")' -i}

displays ``hi'' on start-up, but still presents a @tech{REPL}.

If module-requiring flags appear before @Flag{i}/@DFlag{repl}, they
cancel the automatic requiring of @schememodname[scheme/init]. This
behavior can be used to initialize the @tech{REPL}'s environment with
a different language. For example,

@commandline{mzscheme -l scheme/base -i}

starts a @tech{REPL} using a much smaller initial language (that loads
much faster). Beware that most modules do not provide the basic syntax
of Scheme, including function-call syntax and @scheme[require]. For
example,

@commandline{mzscheme -l scheme/date -i}

produces a @tech{REPL} that fails for every expression, because
@schememodname[scheme/date] provides only a few functions, and not the
@scheme[#%top-interaction] and @scheme[#%app] bindings that are needed
to evaluate top-level function calls in the @tech{REPL}.

If a module-requiring flag appears after @Flag{i}/@DFlag{repl} instead
of before it, then the module is required after
@schememodname[scheme/init] to augment the initial environment. For
example,

@commandline{mzscheme -i -l scheme/date}

starts a useful @tech{REPL} with @schememodname[scheme/date] available
in addition to the exports of @schememodname[scheme].

@; ----------------------------------------

@subsection[#:tag "start-module-mode"]{Module Mode}

If a file argument is supplied to @exec{mzscheme} before any
command-line switch (other than configuration options), then the file
is required as a module, and (unless @Flag{i}/@DFlag{repl} is
specified), no @tech{REPL} is started. For example,

@commandline{mzscheme hello.ss}

requires the @filepath{hello.ss} module and then exits. Any argument
after the file name, flag or otherwise, is preserved as a command-line
argument for use by the required module via
@scheme[current-command-line-arguments].

If command-line flags are used, then the @Flag{u} or
@DFlag{require-script} flag can be used to explicitly require a file
as a module.  The @Flag{t} or @DFlag{require} flag is similar, except
that additional command-line flags are processed by @exec{mzscheme},
instead of preserved for the required module. For example,

@commandline{mzscheme -t hello.ss -t goodbye.ss}

requires the @filepath{hello.ss} module, then requires the
@filepath{goodbye.ss} module, and then exits.

The @Flag{l} or @DFlag{lib} flag is similar to
@Flag{t}/@DFlag{require}, but it requires a module using a
@scheme[lib] module path instead of a file path. For example,

@commandline{mzscheme -l compiler}

is the same as running the @exec{mzc} executable with no arguments,
since the @scheme[compiler] module is the main @exec{mzc}
module.

Note that if you wanted to pass command-line flags to
@scheme[compiler] above, you would need to protect the flags with a
@Flag{-}, so that @exec{mzscheme} doesn't try to parse them itself:

@commandline{mzscheme -l compiler -- --make prog.ss}

@; ----------------------------------------

@subsection[#:tag "start-load-mode"]{Load Mode}

The @Flag{f} or @DFlag{load} flag supports @scheme[load]ing top-level
expressions in a file directly, as opposed to expressions within a
module file. This evaluation is like starting a @tech{REPL} and typing
the expressions directly, except that the results are not printed.
For example,

@commandline{mzscheme -f hi.ss}

@scheme[load]s @filepath{hi.ss} and exits. Note that load mode is
generally a bad idea, for the reasons explained in
@secref["use-module"]; using module mode is typically better.

The @Flag{e} or @DFlag{eval} flag accepts an expression to evaluate
directly. Unlike file loading, the result of the expression is
printed, as in a @tech{REPL}. For example,

@commandline{mzscheme -e '(current-seconds)'}

prints the number of seconds since January 1, 1970.

For file loading and expression evaluation, the top-level environment
is created in the same way for
@seclink["start-interactive-mode"]{interactive mode}:
@schememodname[scheme/init] is required unless another module is
specified first. For example,

@commandline{mzscheme -l scheme/base -e '(current-seconds)'}

likely runs faster, because it initializes the environment for
evaluation using the smaller @schememodname[scheme/base] language,
instead of @schememodname[scheme/init].

@; ----------------------------------------------------------------------

@include-section["scripts.scrbl"]

@; ----------------------------------------------------------------------

@section[#:tag "exe"]{Creating Stand-Alone Executables}

@(define mzc-doc '(lib "scribblings/mzc/mzc.scrbl"))

For information on creating and distributing executables, see
@secref[#:doc mzc-doc "sa"] in @other-manual[mzc-doc].
