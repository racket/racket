#lang scribble/doc
@(require "mz.ss"
          scribble/bnf
          (for-label racket/pretty
                     racket/gui/base))

@(define (FlagFirst n) (as-index (Flag n)))
@(define (DFlagFirst n) (as-index (DFlag n)))
@(define (PFlagFirst n) (as-index (PFlag n)))

@(define (nontermstr s)
   @elem{@schemevalfont{"}@nonterm[s]@schemevalfont{"}})

@(define eventspace
   @tech[#:doc '(lib "scribblings/gui/gui.scrbl")]{eventspace})

@title[#:tag "running-sa"]{Running MzScheme or MrEd}

The core PLT Scheme run-time system is available in two main variants:

@itemize[

 @item{MzScheme, which provides the primitives libraries on which
       @schememodname[racket/base] is implemented. Under Unix and Mac
       OS X, the executable is called
       @as-index{@exec{mzscheme}}. Under Windows, the executable is
       called @as-index{@exec{MzScheme.exe}}.}

 @item{MrEd, which extends @exec{mzscheme} with GUI primitives on
       which @schememodname[racket/gui/base] is implemented. Under
       Unix, the executable is called @as-index{@exec{mred}}. Under
       Windows, the executable is called
       @as-index{@exec{MrEd.exe}}. Under Mac OS X, the @exec{mred}
       script launches @as-index{@exec{MrEd.app}}.}

]

@; ----------------------------------------------------------------------

@section[#:tag "init-actions"]{Initialization}

On startup, the top-level environment contains no bindings---not even
@scheme[#%app] for function application. Primitive modules with names
that start with @schemeidfont{#%} are defined, but they are not meant
for direct use, and the set of such modules can change.  For example,
the @indexed-scheme['#%kernel] module is eventually used to bootstrap
the implemetation of @schememodname[racket/base], and
@scheme['#%mred-kernel] is used for @schememodname[racket/gui/base].

The first action of MzScheme or MrEd is to initialize
@scheme[current-library-collection-paths] to the result of
@scheme[(find-library-collection-paths _pre-extras _extras)], where
@scheme[_pre-extras] is normally @scheme[null] and @scheme[_extras]
are extra directory paths provided in order in the command line with
@Flag{S}/@DFlag{search}. An executable created from the MzScheme or
MrEd executable can embed paths used as @scheme[_pre-extras].

MzScheme and MrEd next @scheme[require] @schememodname[racket/init]
and @schememodname[racket/gui/init], respectively, but only if the
command line does not specify a @scheme[require] flag
(@Flag{t}/@DFlag{require}, @Flag{l}/@DFlag{lib}, or
@Flag{u}/@DFlag{require-script}) before any @scheme[eval],
@scheme[load], or read-eval-print-loop flag (@Flag{e}/@DFlag{eval},
@Flag{f}/@DFlag{load}, @Flag{r}/@DFlag{script}, @Flag{m}/@DFlag{main},
or @Flag{i}/@DFlag{repl}). The initialization library can be changed
with the @Flag{I} @tech{configuration option}. The
@scheme['configure-runtime] property of the initialization library's
language is used before the library is instantiated; see
@secref["configure-runtime"].

After potentially loading the initialization module, expression
@scheme[eval]s, files @scheme[load]s, and module @scheme[require]s are
executed in the order that they are provided on the command line. If
any raises an uncaught exception, then the remaining @scheme[eval]s,
@scheme[load]s, and @scheme[require]s are skipped. If the first
@scheme[require] precedes any @scheme[eval] or @scheme[load] so that
the initialization library is skipped, then the
@scheme['configure-runtime] property of the required module's library
language is used before the module is instantiated; see
@secref["configure-runtime"].

After running all command-line expressions, files, and modules,
MzScheme or MrEd then starts a read-eval-print loop for interactive
evaluation if no command line flags are provided other than
@tech{configuration options}.  If any command-line argument is
provided that is not a @tech{configuration option}, then the
read-eval-print-loop is not started, unless the @Flag{i}/@DFlag{repl}
flag is provided on the command line to
specifically re-enable it. In addition, just before the command line
is started, MzScheme loads the file @scheme[(find-system-path
'init-file)] and MrEd loads the file
@scheme[(find-graphical-system-path 'init-file)] is loaded, unless the
@Flag{q}/@DFlag{no-init-file} flag is specified on the command line.

Finally, before MrEd exists, it waits for all frames to class, all
timers to stop, @|etc| in the main @|eventspace| by evaluating
@scheme[(scheme 'yield)]. This waiting step can be suppressed with the
@Flag{V}/@DFlag{no-yield} command-line flag.

@; ----------------------------------------------------------------------

@section[#:tag "exit-status"]{Exit Status}

The default exit status for a MzScheme or MrEd process is non-zero if
an error occurs during a command-line @scheme[eval] (via @Flag{e},
etc.), @scheme[load] (via @Flag{f}, @Flag{r}, etc.), or
@scheme[require] (via @Flag{-l}, @Flag{t}, etc.), but only when no
read-eval-print loop is started. Otherwise, the default exit status is
@scheme[0].

In all cases, a call to @scheme[exit] (when the default @tech{exit
handler} is in place) can end the process with a specific status
value.

@; ----------------------------------------------------------------------

@include-section["init.scrbl"]

@; ----------------------------------------------------------------------

@section[#:tag "mz-cmdline"]{Command Line}

The MzScheme and MrEd executables recognize the following command-line
flags:

@itemize[

 @item{File and expression options:

 @itemize[

  @item{@FlagFirst{e} @nonterm{expr} or @DFlagFirst{eval}
        @nonterm{expr} : @scheme[eval]s @nonterm{expr}. The results of
        the evaluation are printed via @scheme[current-print].}

  @item{@FlagFirst{f} @nonterm{file} or @DFlagFirst{load}
        @nonterm{file} : @scheme[load]s @nonterm{file}.}

  @item{@FlagFirst{t} @nonterm{file} or @DFlagFirst{require}
        @nonterm{file} : @scheme[require]s @nonterm{file}.}

  @item{@FlagFirst{l} @nonterm{path} or @DFlagFirst{lib}
       @nonterm{path} : @scheme[require]s @scheme[(lib
       @#,nontermstr{path})].}

  @item{@FlagFirst{p} @nonterm{package} :
       @scheme[require]s @scheme[(planet @#,nontermstr{package})].

       @margin-note{Despite its name, @DFlag{script} is not usually
       used for Unix scripts. See @guidesecref["scripts"] for more
       information on scripts.}}

  @item{@FlagFirst{r} @nonterm{file} or @DFlagFirst{script}
        @nonterm{file} : @scheme[load]s @nonterm{file} as a
        script. This flag is like @Flag{t} @nonterm{file} plus
        @Flag{N} @nonterm{file} to set the program name and @Flag{-}
        to cause all further command-line elements to be treated as
        non-flag arguments.}

  @item{@FlagFirst{u} @nonterm{file} or @DFlagFirst{require-script}
       @nonterm{file} : @scheme[require]s @nonterm{file} as a script;
       This flag is like @Flag{t} @nonterm{file} plus @Flag{N}
       @nonterm{file} to set the program name and @Flag{-} to cause
       all further command-line elements to be treated as non-flag
       arguments.}

  @item{@FlagFirst{k} @nonterm{n} @nonterm{m} : Loads code embedded in
        the executable from file position @nonterm{n} to
        @nonterm{m}. This option is normally embedded in a stand-alone
        binary that also embeds Scheme code.}

  @item{@FlagFirst{m} or @DFlagFirst{main} : Evaluates a call to
        @schemeidfont{main} as bound in the top-level environment. All
        of the command-line arguments that are not processed as
        options (i.e., the arguments put into
        @scheme[current-command-line-arguments]) are passed as
        arguments to @schemeidfont{main}. The results of the call are
        printed via @scheme[current-print].

        The call to @schemeidfont{main} is constructed as an
        expression @scheme[((unsyntax @schemeidfont{main}) _arg-str
        ...)]  where the lexical context of the expression gives
        @schemeidfont{#%app} and @schemeidfont{#%datum} bindings as
        @scheme[#%plain-app] and @scheme[#%datum], but the lexical
        context of @schemeidfont{main} is the top-level environment.}

 ]}

 @item{Interaction options:

 @itemize[

  @item{@FlagFirst{i} or @DFlagFirst{repl} : Runs interactive read-eval-print
        loop, using either @scheme[read-eval-print-loop] (MzScheme) or
        @scheme[graphical-read-eval-print-loop] (MrEd) after showing
        @scheme[(banner)] and loading @scheme[(find-system-path
        'init-file)]. For MrEd, supply the @Flag{z}/@DFlag{text-repl}
        configuration option to use @scheme[read-eval-print-loop]
        instead of @scheme[graphical-read-eval-print-loop].}

  @item{@FlagFirst{n} or @DFlagFirst{no-lib} : Skips requiring the
        initialization library (i.e., @schememodname[racket/init] or
        @schememodname[racket/gui/init], unless it is changed with the
        @Flag{I} flag) when not otherwise disabled.}

  @item{@FlagFirst{v} or @DFlagFirst{version} : Shows
        @scheme[(banner)].}

  @item{@FlagFirst{K} or @DFlagFirst{back} : MrEd, Mac OS X only;
        leave application in the background.}

  @item{@FlagFirst{V} @DFlagFirst{no-yield} : Skips final
        @scheme[(yield 'wait)] action, which normally waits until all
        frames are closed, @|etc| in the main @|eventspace| before
        exiting.}

 ]}

 @item{@deftech{Configuration options}:

 @itemize[

  @item{@FlagFirst{c} or @DFlagFirst{no-compiled} : Disables loading
        of compiled byte-code @filepath{.zo} files, by initializing
        @scheme[current-compiled-file-paths] to @scheme[null].}

  @item{@FlagFirst{q} or @DFlagFirst{no-init-file} : Skips loading
        @scheme[(find-system-path 'init-file)] for
        @Flag{i}/@DFlag{repl}.}

  @item{@FlagFirst{z} or @DFlagFirst{text-repl} : MrEd only; changes
        @Flag{i}/@DFlag{repl} to use
        @scheme[textual-read-eval-print-loop] instead of
        @scheme[graphical-read-eval-print-loop].}

  @item{@FlagFirst{I} @nonterm{path} : Sets @scheme[(lib
        @#,nontermstr{path})] as the path to @scheme[require] to initialize
        the namespace, unless namespace initialization is disabled.}

  @item{@FlagFirst{X} @nonterm{dir} or @DFlagFirst{collects}
        @nonterm{dir} : Sets @nonterm{dir} as the path to the main
        collection of libraries by making @scheme[(find-system-path
        'collects-dir)] produce @nonterm{dir}.}

  @item{@FlagFirst{S} @nonterm{dir} or @DFlagFirst{search}
        @nonterm{dir} : Adds @nonterm{dir} to the default library
        collection search path after the main collection directory. If
        the @Flag{S}/@DFlag{dir} flag is supplied multiple times, the
        search order is as supplied.}

  @item{@FlagFirst{U} or @DFlagFirst{no-user-path} : Omits
        user-specific paths in the search for collections, C
        libraries, etc. by initializing the
        @scheme[use-user-specific-search-paths] parameter to
        @scheme[#f].}

  @item{@FlagFirst{N} @nonterm{file} or @DFlagFirst{name}
        @nonterm{file} : sets the name of the executable as reported
        by @scheme[(find-system-path 'run-file)] to
        @nonterm{file}.}

  @item{@FlagFirst{j} or @DFlagFirst{no-jit} : Disables the
        native-code just-in-time compiler by setting the
        @scheme[eval-jit-enabled] parameter to @scheme[#f].}

  @item{@FlagFirst{d} or @DFlagFirst{no-delay} : Disables on-demand
        parsing of compiled code and syntax objects by setting the
        @scheme[read-on-demand-source] parameter to @scheme[#f].}

  @item{@FlagFirst{b} or @DFlagFirst{binary} : Requests binary mode,
        instead of text mode, for the process's input, out, and error
        ports. This flag currently has no effect, because binary mode
        is always used.}

  @item{@FlagFirst{W} @nonterm{level} or @DFlagFirst{warn}
        @nonterm{level} : Sets the logging level for writing events to
        the original error port. The possible @nonterm{level} values
        are the same as for the @envvar{PLTSTDERR} environment
        variable. See @secref["logging"] for more information.}

  @item{@FlagFirst{L} @nonterm{level} or @DFlagFirst{syslog}
        @nonterm{level} : Sets the logging level for writing events to
        the system log. The possible @nonterm{level} values
        are the same as for the @envvar{PLTSYSLOG} environment
        variable. See @secref["logging"] for more information.}

 ]}

 @item{Meta options:

 @itemize[

  @item{@FlagFirst{-} : No argument following this flag is itself used
        as a flag.}
 
  @item{@FlagFirst{h} or @DFlagFirst{help} : Shows information about
        the command-line flags and start-up process and exits,
        ignoring all other flags.}
 
 ]}

]

If at least one command-line argument is provided, and if the first
one after any @tech{configuration option} is not a flag, then a
@Flag{u}/@DFlag{require-script} flag is implicitly added before the
first non-flag argument.

If no command-line arguments are supplied other than
@tech{configuration options}, then the @Flag{i}/@DFlag{repl} flag is
effectively added.

For MrEd under X11, the follow flags are recognized when they appear
at the beginning of the command line, and they count as configuration
options (i.e., they do not disable the read-eval-print loop or prevent
the insertion of @Flag{u}/@DFlag{require-script}):

@itemize[

  @item{@FlagFirst{display} @nonterm{display} : Sets the X11 display
        to use.}

  @item{@FlagFirst{geometry} @nonterm{arg}, @FlagFirst{bg}
        @nonterm{arg}, @FlagFirst{background} @nonterm{arg},
        @FlagFirst{fg} @nonterm{arg}, @FlagFirst{foreground}
        @nonterm{arg}, @FlagFirst{fn} @nonterm{arg}, @FlagFirst{font}
        @nonterm{arg}, @FlagFirst{iconic}, @FlagFirst{name}
        @nonterm{arg}, @FlagFirst{rv}, @FlagFirst{reverse},
        @PFlagFirst{rv}, @FlagFirst{selectionTimeout} @nonterm{arg},
        @FlagFirst{synchronous}, @FlagFirst{title} @nonterm{arg},
        @FlagFirst{xnllanguage} @nonterm{arg}, or @FlagFirst{xrm}
        @nonterm{arg} : Standard X11 arguments that are mostly ignored
        but accepted for compatibility with other X11 programs. The
        @Flag{synchronous} and @Flag{xrm} flags behave in the usual
        way.}

  @item{@FlagFirst{singleInstance} : If an existing MrEd is already
        running on the same X11 display, if it was started on a
        machine with the same hostname, and if it was started with the
        same name as reported by @scheme[(find-system-path
        'run-file)]---possibly set with the @Flag{N}/@DFlag{name}
        command-line argument---then all non-option command-line
        arguments are treated as filenames and sent to the existing
        MrEd instance via the application file handler (see
        @scheme[application-file-handler]).}

]

Similarly, under Mac OS X, a leading switch starting with
@FlagFirst{psn_} is treated as a special configuration option. It
indicates that Finder started the application, so the current input,
output, and error output are redirected to a GUI window.

Multiple single-letter switches (the ones preceded by a single
@litchar{-}) can be collapsed into a single switch by concatenating
the letters, as long as the first switch is not @Flag{-}. The
arguments for each switch are placed after the collapsed switches (in
the order of the switches). For example,

@commandline{-ifve @nonterm{file} @nonterm{expr}}

and

@commandline{-i -f @nonterm{file} -v -e @nonterm{expr}}

are equivalent. If a collapsed @Flag{-} appears before other collapsed
switches in the same collapsed set, it is implicitly moved to the end
of the collapsed set.

Extra arguments following the last option are available from the
@indexed-scheme[current-command-line-arguments] parameter.

@; ----------------------------------------------------------------------

@section[#:tag "configure-runtime"]{Language Run-Time Configuration}

When a module is implemented using @hash-lang{}, the language after
@hash-lang{} can specify configuration actions to perform when a
module using the language is the main module of a program. The
language specifies run-time configuration by

@itemlist[

 @item{attaching a @scheme['module-language] @tech{syntax property} to
       the module as read from its source (see @scheme[module] and
       @scheme[module-compiled-language-info]);}

 @item{having the function indicated by the @scheme['module-language]
       @tech{syntax property} recognize the
       @scheme['configure-runtime] key, for which it returns a list of
       vectors; each vector must have the form @scheme[(vector _mp
       _name _val)] where @scheme[_mp] is a @tech{module path},
       @scheme[_name] is a symbol, and @scheme[_val] is an arbitrary
       value; and}

 @item{having each function called as @scheme[((dynamic-require _mp
       _name) _val)] configure the run-time environment, typically by
       setting parameters such as @scheme[current-print].}

]

The @schememodname[racket/base] and @schememodname[scheme] languages
do not currently specify a run-time configuration action.

A @scheme['configure-runtime] query returns a list of vectors, instead
of directly configuring the environment, so that the indicated modules
to be bundled with a program when creating a stand-alone
executable; see @secref[#:doc '(lib "scribblings/mzc/mzc.scrbl") "exe"].

For information on defining a new @hash-lang[] language, see
@schememodname[syntax/module-reader].
