#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          "common.rkt" 
          (for-label racket/base
                     racket/runtime-path
                     compiler/embed
                     launcher/launcher))

@(define (gtech s) (tech #:doc '(lib "scribblings/guide/guide.scrbl") s))

@title[#:tag "exe"]{@exec{raco exe}: Creating Stand-Alone Executables}

@margin-note{To achieve a faster startup time, instead of trying
@exec{raco exe}, use a smaller base language---such as
@racketmodfont{#lang} @racketmodname[racket/base] instead of
@racketmodfont{#lang} @racketmodname[racket]. Also, ensure that
bytecode files are compiled by using @seclink["make"]{@exec{raco make}}.}

Compiled code produced by @exec{raco make} relies on Racket
executables to provide run-time support to the compiled code. However,
@exec{raco exe} can package code together with its run-time support to
form an executable, and @exec{raco distribute} can package the
executable into a distribution that works on other machines. Running
an executable produced by @exec{raco exe} will not improve performance
over @exec{raco make}.

The @exec{raco exe} command embeds a module, from source or byte
code, into a copy of the @exec{racket} executable. (On Unix, the
embedding executable is actually a copy of a wrapper executable.)  The
created executable invokes the embedded module on startup. The
@DFlag{gui} flag causes the program to be embedded in a copy of the
@exec{gracket} executable. If the embedded module refers to other
modules via @racket[require], then the other modules are also included
in the embedding executable.

For example, the command

@commandline{raco exe --gui hello.rkt}

produces either @filepath{hello.exe} (Windows), @filepath{hello.app}
(Mac OS X), or @filepath{hello} (Unix), which runs the same as running
the @filepath{hello.rkt} module in @exec{gracket}.

Library modules or other files that are referenced
dynamically---through @racket[eval], @racket[load], or
@racket[dynamic-require]---are not automatically embedded into the
created executable. Such modules can be explicitly included using the
@DPFlag{lib} flag to @exec{raco exe}. Alternately, use
@racket[define-runtime-path] to embed references to the run-time files
in the executable; the files are then copied and packaged together
with the executable when creating a distribution (as described in
@secref["exe-dist"]). Finally, a submodule is included if its
enclosing module is included and the submodule contains a
sub-submodule named @racketidfont{declare-preserve-for-embedding}
(where the implementation of the sub-submodule is ignored).

Modules that are implemented directly by extensions---i.e., extensions
that are automatically loaded from @racket[(build-path "compiled"
"native" (system-library-subpath))] to satisfy a
@racket[require]---are treated like other run-time files: a generated
executable uses them from their original location, and they are copied
and packaged together when creating a distribution.

The @exec{raco exe} command works only with module-based programs. The
@racketmodname[compiler/embed] library provides a more general
interface to the embedding mechanism.

A stand-alone executable is ``stand-alone'' in the sense that you can
run it without starting @exec{racket}, @exec{gracket}, or
DrRacket. However, the executable depends on Racket shared libraries,
and possibly other run-time files declared via
@racket[define-runtime-path]. The executable can be packaged with
support libraries to create a distribution using @exec{raco
distribute}, as described in @secref["exe-dist"].

The @exec{raco exe} command accepts the following command-line flags:

@itemlist[

 @item{@Flag{o} @nonterm{file} --- create the executable as
   @nonterm{file}, adding a suffix to @nonterm{file} as appropriate
   for the platform and executable type. On Mac OS X in @DFlag{gui}
   mode, @nonterm{file} is actually a bundle directory, but it appears
   as a file within Finder.}

 @item{@DFlag{gui} --- create a graphical executable based on
   @exec{gracket} instead of @exec{racket}.}

 @item{@Flag{l} or @DFlag{launcher} --- create a @tech{launcher} (see
   @secref["launcher"]), instead of a stand-alone executable. Flags
   such as @DFlag{config-path}, @DFlag{collects-path}, and @DFlag{lib}
   have no effect on launchers. Beware that the default command-line
   flags to build into the launcher prevent access to packages that
   are installed in user scope; use @exec{--exf -U} to enable access
   to user-scope packages from the launcher.}

 @item{@DFlag{config-path} @nonterm{path} --- set @nonterm{path}
   within the executable as the path to the @tech{configuration
   directory}; if the path is relative, it will be treated as relative
   to the executable. The default path is @filepath{etc}, with the
   expectation that no such directory will exist at run time.}

 @item{@DFlag{collects-path} @nonterm{path} --- set @nonterm{path}
   within the executable as the path to the @tech{main collection
   directory}; if the path is relative, it will be treated as relative
   to the executable. The default is to have no path, which means that
   the @racket[current-library-collection-paths] and
   @racket[current-library-collection-links] parameters are
   initialized at empty when the executable starts. Beware that
   various other directories are located relative to the @tech{main
   collection directory} by default (see @secref["config-file"]), so
   that installing @nonterm{path} may allow other directories to be
   found---intentional or not.}

 @item{@DFlag{collects-dest} @nonterm{path} --- write modules to be
   included with the executable into @nonterm{path}
   (relative to the current directory), instead of embedded within the
   executable. The @DFlag{collects-dest} flag normally makes sense
   only in combination with @DFlag{collects-path}. This mode currently
   does not prune unreferenced submodules (and it pulls along any
   dependencies of submodules).}

 @item{@DFlag{ico} @nonterm{.ico-path} --- on Windows, set the icons
   for the generated executable to ones extracted from
   @nonterm{.ico-path}; see @racket[create-embedding-executable]'s
   use of the @racket['ico] auxiliary association for more information
   about expected icon sizes and transformations.}

 @item{@DFlag{icns} @nonterm{.icns-path} --- on Mac OS X, set the icons
   for the generated executable to be the content of
   @nonterm{.icns-path}.}

 @item{@DFlag{orig-exe} --- generate an executable that refers to
   the original @exec{racket} or @exec{gracket} executable,
   instead of making a copy. This flag is rarely useful, because the
   part of the executable that is copied is normally small, and
   @exec{raco distribute} does not work with executables that are
   created with @DFlag{orig-exe}.}

 @item{@DFlag{3m} --- generate an executable based on the @gtech{3m}
   variant of Racket, which is the default unless running a @exec{raco
   exe} that is based on the @gtech{CGC} variant.}

 @item{@DFlag{cgc} --- generate an executable based on the @gtech{CGC}
   variant of Racket, which is the default only when running a
   @exec{raco exe} that is based on the @gtech{CGC} variant.}

 @item{@DPFlag{aux} @nonterm{file} --- attach information to the
   executable based on @nonterm{file}'s suffix; see
   @racket[extract-aux-from-path] for a list of recognized suffixes
   and meanings, and see @racket[create-embedding-executable]'s use of
   auxiliary association for more specific information about how each
   kind of file is used.}

 @item{@DPFlag{lib} @nonterm{module-path} --- include @nonterm{module-path}
   in the executable, even if it is not referenced by the main program,
   so that it is available via @racket[dynamic-require].}

 @item{@DPFlag{exf} @nonterm{flag} --- provide the @nonterm{flag}
   command-line argument on startup to the embedded @exec{racket} or
   @exec{gracket}.}

 @item{@DFlag{exf} @nonterm{flag} --- remove @nonterm{flag} from the
   command-line arguments to be provided on startup to the embedded
   @exec{racket} or @exec{gracket}.}

 @item{@DFlag{exf-clear} --- remove all command-line arguments to be
   provided on startup to the embedded @exec{racket} or
   @exec{gracket}.}

 @item{@DFlag{exf-show} --- show (without changing) the command-line
   arguments to be provided on startup to the embedded
   @exec{racket} or @exec{gracket}.}

 @item{@Flag{v} --- report progress verbosely.}

 @item{@DFlag{vv} --- report progress more verbosely than @Flag{v}.}

]

@history[#:changed "6.3.0.11" @elem{Added support for
                                    @racketidfont{declare-preserve-for-embedding}.}]

@; ----------------------------------------------------------------------

@include-section["exe-api.scrbl"]
@include-section["launcher.scrbl"]
@include-section["exe-dylib-path.scrbl"]

