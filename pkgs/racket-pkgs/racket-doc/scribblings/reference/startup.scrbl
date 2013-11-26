#lang scribble/doc
@(require "mz.rkt" scribble/bnf (for-label racket/pretty racket/gui/base))

@(define (FlagFirst n) (as-index (Flag n)))
@(define (DFlagFirst n) (as-index (DFlag n)))
@(define (PFlagFirst n) (as-index (PFlag n)))

@(define (nontermstr s)
   @elem{@racketvalfont{"}@nonterm[s]@racketvalfont{"}})

@(define eventspace
   @tech[#:doc '(lib "scribblings/gui/gui.scrbl")]{eventspace})

@title[#:tag "running-sa"]{Running Racket or GRacket}

The core Racket run-time system is available in two main variants:

@itemize[

 @item{Racket, which provides the primitives libraries on which
       @racketmodname[racket/base] is implemented. On Unix and Mac
       OS X, the executable is called
       @as-index{@exec{racket}}. On Windows, the executable is
       called @as-index{@exec{Racket.exe}}.}

 @item{GRacket, which is a GUI variant of @exec{racket} to the degree
       that the system distinguishes them. On Unix, the executable
       is called @as-index{@exec{gracket}}, and single-instance flags
       and X11-related flags are handled and communicated specially to
       the @racket[racket/gui/base] library. On Windows, the
       executable is called @as-index{@exec{GRacket.exe}}, and it is a
       GUI application (as opposed to a console application) that
       implements singe-instance support. On Mac OS X, the
       @exec{gracket} script launches @as-index{@exec{GRacket.app}}.}

]

@; ----------------------------------------------------------------------

@section[#:tag "init-actions"]{Initialization}

On start-up, the top-level environment contains no bindings---not even
@racket[#%app] for function application. Primitive modules with names
that start with @racketidfont{#%} are defined, but they are not meant
for direct use, and the set of such modules can change.  For example,
the @indexed-racket['#%kernel] module is eventually used to bootstrap
the implementation of @racketmodname[racket/base].

The first action of Racket or GRacket is to initialize
@racket[current-library-collection-paths] to the result of
@racket[(find-library-collection-paths _pre-extras _extras)], where
@racket[_pre-extras] is normally @racket[null] and @racket[_extras]
are extra directory paths provided in order in the command line with
@Flag{S}/@DFlag{search}. An executable created from the Racket or
GRacket executable can embed paths used as @racket[_pre-extras].

Racket and GRacket next @racket[require] @racketmodname[racket/init]
and @racketmodname[racket/gui/init], respectively, but only if the
command line does not specify a @racket[require] flag
(@Flag{t}/@DFlag{require}, @Flag{l}/@DFlag{lib}, or
@Flag{u}/@DFlag{require-script}) before any @racket[eval],
@racket[load], or read-eval-print-loop flag (@Flag{e}/@DFlag{eval},
@Flag{f}/@DFlag{load}, @Flag{r}/@DFlag{script}, @Flag{m}/@DFlag{main},
or @Flag{i}/@DFlag{repl}). The initialization library can be changed
with the @Flag{I} @tech{configuration option}. The
@racket[configure-runtime] submodule of the initialization library or the
@racket['configure-runtime] property of the initialization library's
language is used before the library is instantiated; see
@secref["configure-runtime"].

After potentially loading the initialization module, expression
@racket[eval]s, files @racket[load]s, and module @racket[require]s are
executed in the order that they are provided on the command line. If
any raises an uncaught exception, then the remaining @racket[eval]s,
@racket[load]s, and @racket[require]s are skipped. If the first
@racket[require] precedes any @racket[eval] or @racket[load] so that
the initialization library is skipped, then the @racket[configure-runtime]
submodule of the required module or the
@racket['configure-runtime] property of the required module's library
language is used before the module is instantiated; see
@secref["configure-runtime"].

After running all command-line expressions, files, and modules,
Racket or GRacket then starts a read-eval-print loop for interactive
evaluation if no command line flags are provided other than
@tech{configuration options}.  If any command-line argument is
provided that is not a @tech{configuration option}, then the
read-eval-print-loop is not started, unless the @Flag{i}/@DFlag{repl}
flag is provided on the command line to
specifically re-enable it. In addition, just before the command line
is started, Racket loads the file @racket[(find-system-path
'init-file)] and GRacket loads the file
@racket[(find-graphical-system-path 'init-file)] is loaded, unless the
@Flag{q}/@DFlag{no-init-file} flag is specified on the command line.

Finally, before Racket or GRacket exits, it calls the procedure that
is the current value of @racket[executable-yield-handler] in the main
thread, unless the @Flag{V}/@DFlag{no-yield} command-line flag is
specified. Requiring @racketmodname[racket/gui/base] sets this parameter call
@racket[(racket 'yield)].

@; ----------------------------------------------------------------------

@section[#:tag "exit-status"]{Exit Status}

The default exit status for a Racket or GRacket process is non-zero if
an error occurs during a command-line @racket[eval] (via @Flag{e},
etc.), @racket[load] (via @Flag{f}, @Flag{r}, etc.), or
@racket[require] (via @Flag{-l}, @Flag{t}, etc.), but only when no
read-eval-print loop is started. Otherwise, the default exit status is
@racket[0].

In all cases, a call to @racket[exit] (when the default @tech{exit
handler} is in place) can end the process with a specific status
value.

@; ----------------------------------------------------------------------

@include-section["init.scrbl"]

@; ----------------------------------------------------------------------

@section[#:tag "mz-cmdline"]{Command Line}

The Racket and GRacket executables recognize the following command-line
flags:

@itemize[

 @item{File and expression options:

 @itemize[

  @item{@FlagFirst{e} @nonterm{expr} or @DFlagFirst{eval}
        @nonterm{expr} : @racket[eval]s @nonterm{expr}. The results of
        the evaluation are printed via @racket[current-print].}

  @item{@FlagFirst{f} @nonterm{file} or @DFlagFirst{load}
        @nonterm{file} : @racket[load]s @nonterm{file}; if
        @nonterm{file} is @filepath{-}, then expressions are read and
        evaluated from standard input.}

  @item{@FlagFirst{t} @nonterm{file} or @DFlagFirst{require}
        @nonterm{file} : @racket[require]s @nonterm{file}, and then
        @racket[require]s @racket[(submod (file @#,nontermstr{file})
        main)] if available.}

  @item{@FlagFirst{l} @nonterm{path} or @DFlagFirst{lib}
       @nonterm{path} : @racket[require]s @racket[(lib
       @#,nontermstr{path})], and then @racket[require]s
       @racket[(submod (lib @#,nontermstr{path}) main)] if available.}

  @item{@FlagFirst{p} @nonterm{package} :
       @racket[require]s @racket[(planet @#,nontermstr{package})],
       and then 
        @racket[require]s @racket[(submod (planet @#,nontermstr{package})
        main)] if available.}

  @item{@FlagFirst{r} @nonterm{file} or @DFlagFirst{script}
        @nonterm{file} : @racket[load]s @nonterm{file}
       @margin-note*{Despite its name, @DFlag{script} is not usually
       used for Unix scripts. See @guidesecref["scripts"] for more
       information on scripts.}
        as a script. This flag is like @Flag{t} @nonterm{file} plus
        @Flag{N} @nonterm{file} to set the program name and @Flag{-}
        to cause all further command-line elements to be treated as
        non-flag arguments.}

  @item{@FlagFirst{u} @nonterm{file} or @DFlagFirst{require-script}
       @nonterm{file} : @racket[require]s @nonterm{file} as a script;
       This flag is like @Flag{t} @nonterm{file} plus @Flag{N}
       @nonterm{file} to set the program name and @Flag{-} to cause
       all further command-line elements to be treated as non-flag
       arguments.}

  @item{@FlagFirst{k} @nonterm{n} @nonterm{m} @nonterm{p} : Loads code
        embedded in the executable from file position @nonterm{n} to
        @nonterm{m} and from @nonterm{m} to @nonterm{p}. (On Mac OS X,
        @nonterm{n}, @nonterm{m}, and @nonterm{p} are relative to a
        @tt{__PLTSCHEME} segment in the executable.) The first range
        is loaded in every new @tech{place}, and any modules declared
        in that range are considered predefined in the sense of
        @racket[module-predefined?]. This option is normally embedded
        in a stand-alone binary that also embeds Racket code.}

  @item{@FlagFirst{m} or @DFlagFirst{main} : Evaluates a call to
        @racketidfont{main} as bound in the top-level environment. All
        of the command-line arguments that are not processed as
        options (i.e., the arguments put into
        @racket[current-command-line-arguments]) are passed as
        arguments to @racketidfont{main}. The results of the call are
        printed via @racket[current-print].

        The call to @racketidfont{main} is constructed as an
        expression @racket[((unsyntax @racketidfont{main}) _arg-str
        ...)]  where the lexical context of the expression gives
        @racketidfont{#%app} and @racketidfont{#%datum} bindings as
        @racket[#%plain-app] and @racket[#%datum], but the lexical
        context of @racketidfont{main} is the top-level environment.}

 ]}

 @item{Interaction options:

 @itemize[

  @item{@FlagFirst{i} or @DFlagFirst{repl} : Runs an interactive read-eval-print
        loop, using either @racket[read-eval-print-loop] (Racket) or
        @racket[graphical-read-eval-print-loop] (GRacket) after showing
        @racket[(banner)] and loading @racket[(find-system-path
        'init-file)]. In the case of Racket, @racket[(read-eval-print-loop)]
        is followed by @racket[(newline)]. For GRacket, supply the @Flag{z}/@DFlag{text-repl}
        configuration option to use @racket[read-eval-print-loop] (and @racket[newline])
        instead of @racket[graphical-read-eval-print-loop].}

  @item{@FlagFirst{n} or @DFlagFirst{no-lib} : Skips requiring the
        initialization library (i.e., @racketmodname[racket/init] or
        @racketmodname[racket/gui/init], unless it is changed with the
        @Flag{I} flag) when not otherwise disabled.}

  @item{@FlagFirst{v} or @DFlagFirst{version} : Shows
        @racket[(banner)].}

  @item{@FlagFirst{K} or @DFlagFirst{back} : GRacket, Mac OS X only;
        leave application in the background.}

  @item{@FlagFirst{V} @DFlagFirst{no-yield} : Skips final
        @racket[executable-yield-handler] action, which normally waits until all
        frames are closed, @|etc| in the main @|eventspace| before
        exiting for programs that use @racketmodname[racket/gui/base].}

 ]}

 @item{@deftech{Configuration options}:

 @itemize[

  @item{@FlagFirst{c} or @DFlagFirst{no-compiled} : Disables loading
        of compiled byte-code @filepath{.zo} files, by initializing
        @racket[current-compiled-file-paths] to @racket[null].
        Use judiciously: this effectively ignores the content of all
        @filepath{compiled} subdirectories, so that any used modules are
        compiled on the fly---even @racketmodname[racket/base] and
        its dependencies---which leads to prohibitively expensive
        run times.}

  @item{@FlagFirst{q} or @DFlagFirst{no-init-file} : Skips loading
        @racket[(find-system-path 'init-file)] for
        @Flag{i}/@DFlag{repl}.}

  @item{@FlagFirst{z} or @DFlagFirst{text-repl} : GRacket only; changes
        @Flag{i}/@DFlag{repl} to use
        @racket[textual-read-eval-print-loop] instead of
        @racket[graphical-read-eval-print-loop].}

  @item{@FlagFirst{I} @nonterm{path} : Sets @racket[(lib
        @#,nontermstr{path})] as the path to @racket[require] to initialize
        the namespace, unless namespace initialization is disabled. Using
        this flag can effectively set the language for the read-eval-print
        loop and other top-level evaluation.}

  @item{@FlagFirst{X} @nonterm{dir} or @DFlagFirst{collects}
        @nonterm{dir} : Sets @nonterm{dir} as the path to the main
        collection of libraries by making @racket[(find-system-path
        'collects-dir)] produce @nonterm{dir}. If @nonterm{dir} is an
        empty string, then @racket[(find-system-path 'collects-dir)]
        returns @filepath{.}, but
        @racket[current-library-collection-paths] is initialized to
        the empty list, and @racket[use-collection-link-paths] is
        initialized to @racket[#f].}

  @item{@FlagFirst{S} @nonterm{dir} or @DFlagFirst{search}
        @nonterm{dir} : Adds @nonterm{dir} to the default library
        collection search path after the main collection directory. If
        the @Flag{S}/@DFlag{dir} flag is supplied multiple times, the
        search order is as supplied.}

  @item{@FlagFirst{R} @nonterm{paths} or @DFlagFirst{compiled}
        @nonterm{paths} : Sets the initial value of the
        @racket[current-compiled-file-roots] parameter, overriding
        any @envvar{PLTCOMPILEDROOTS} setting. The @nonterm{paths}
        argument is parsed in the same way as @envvar{PLTCOMPILEDROOTS}
        (see @racket[current-compiled-file-roots]).}

  @item{@FlagFirst{G} @nonterm{dir} or @DFlagFirst{config}
        @nonterm{dir} : Sets the directory that is returned by
        @racket[(find-system-path 'config-dir)].}

  @item{@FlagFirst{A} @nonterm{dir} or @DFlagFirst{addon}
        @nonterm{dir} : Sets the directory that is returned by
        @racket[(find-system-path 'addon-dir)].}

  @item{@FlagFirst{U} or @DFlagFirst{no-user-path} : Omits
        user-specific paths in the search for collections, C
        libraries, etc. by initializing the
        @racket[use-user-specific-search-paths] parameter to
        @racket[#f].}

  @item{@FlagFirst{N} @nonterm{file} or @DFlagFirst{name}
        @nonterm{file} : sets the name of the executable as reported
        by @racket[(find-system-path 'run-file)] to
        @nonterm{file}.}

  @item{@FlagFirst{J} @nonterm{name} or @DFlagFirst{wm-class}
        @nonterm{name} : GRacket, Unix only; sets the @tt{WM_CLASS}
        program class to @nonterm{name} (while the @tt{WM_CLASS}
        program name is derived from the executable name or a
        @Flag{N}/@DFlag{name} argument).}

  @item{@FlagFirst{j} or @DFlagFirst{no-jit} : Disables the
        native-code just-in-time compiler by setting the
        @racket[eval-jit-enabled] parameter to @racket[#f].}

  @item{@FlagFirst{d} or @DFlagFirst{no-delay} : Disables on-demand
        parsing of compiled code and syntax objects by setting the
        @racket[read-on-demand-source] parameter to @racket[#f].}

  @item{@FlagFirst{b} or @DFlagFirst{binary} : Requests binary mode,
        instead of text mode, for the process's input, out, and error
        ports. This flag currently has no effect, because binary mode
        is always used.}

  @item{@FlagFirst{W} @nonterm{levels} or @DFlagFirst{warn}
        @nonterm{levels} : Sets the logging level for writing events to
        the original error port. The possible @nonterm{level} values
        are the same as for the @envvar{PLTSTDERR} environment
        variable. See @secref["logging"] for more information.}

  @item{@FlagFirst{L} @nonterm{levels} or @DFlagFirst{syslog}
        @nonterm{levels} : Sets the logging level for writing events to
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

For GRacket on Unix, the follow flags are recognized when they appear
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
        @Flag{synchronous} flag behaves in the usual
        way.}

  @item{@FlagFirst{singleInstance} : If an existing GRacket is already
        running on the same X11 display, if it was started on a
        machine with the same hostname, and if it was started with the
        same name as reported by @racket[(find-system-path
        'run-file)]---possibly set with the @Flag{N}/@DFlag{name}
        command-line argument---then all non-option command-line
        arguments are treated as filenames and sent to the existing
        GRacket instance via the application file handler (see
        @racket[application-file-handler]).}

]

Similarly, on Mac OS X, a leading switch starting with
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
@indexed-racket[current-command-line-arguments] parameter.

@; ----------------------------------------------------------------------

@section[#:tag "configure-runtime"]{Language Run-Time Configuration}

@guidealso["module-runtime-config"]

A module can have a @racket[configure-runtime] submodule that is
@racket[dynamic-require]d before the module itself when a module is
the main module of a program. Normally, a @racket[configure-runtime]
submodule is added to a module by the module's language (i.e., by the
@racket[#%module-begin] form among a @racket[module]'s initial
bindings).

Alternatively or in addition, an older protocol is in place.
When a module is implemented using @hash-lang{}, the language after
@hash-lang{} can specify configuration actions to perform when a
module using the language is the main module of a program. The
language specifies run-time configuration by

@itemlist[

 @item{attaching a @racket['module-language] @tech{syntax property} to
       the module as read from its source (see @racket[module] and
       @racket[module-compiled-language-info]);}

 @item{having the function indicated by the @racket['module-language]
       @tech{syntax property} recognize the
       @indexed-racket['configure-runtime] key, for which it returns a list of
       vectors; each vector must have the form @racket[(vector _mp
       _name _val)] where @racket[_mp] is a @tech{module path},
       @racket[_name] is a symbol, and @racket[_val] is an arbitrary
       value; and}

 @item{having each function called as @racket[((dynamic-require _mp
       _name) _val)] configure the run-time environment, typically by
       setting parameters such as @racket[current-print].}

]

A @racket['configure-runtime] query returns a list of vectors, instead
of directly configuring the environment, so that the indicated modules
to be bundled with a program when creating a stand-alone executable;
see @secref[#:doc raco-doc "exe"] in
@other-manual[raco-doc].

For information on defining a new @hash-lang[] language, see
@racketmodname[syntax/module-reader].
