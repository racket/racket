#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          "common.rkt" 
          (for-label racket/base
                     racket/runtime-path
                     compiler/embed
                     launcher/launcher))

@(define (gtech s) (tech #:doc guide-doc s))

@title[#:tag "exe"]{@exec{raco exe}: Creating Stand-Alone Executables}

@margin-note{To achieve a faster startup time, instead of trying
@exec{raco exe}, use a smaller base language---such as
@racketmodfont{#lang} @racketmodname[racket/base] instead of
@racketmodfont{#lang} @racketmodname[racket]. Also, ensure that
bytecode files are compiled by using @seclink["make"]{@exec{raco make}}.}

Compiled code produced by @exec{raco make} relies on Racket
executables to provide run-time support to the compiled code. However,
@exec{raco exe} can package code together with its run-time support to
form an executable, and @seclink["exe-dist"]{@exec{raco distribute}} can package the
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
(Mac OS), or @filepath{hello} (Unix), which runs the same as running
the @filepath{hello.rkt} module in @exec{gracket}.

Library modules or other files that are referenced
dynamically---through @racket[eval], @racket[load], or
@racket[dynamic-require]---are not automatically embedded into the
created executable. Such modules can be explicitly included using the
@DPFlag{lib} flag to @exec{raco exe}. Alternately, use
@racket[define-runtime-path] to embed references to the run-time files
in the executable; the files are then copied and packaged together
with the executable when creating a distribution (as described in
@secref["exe-dist"]). A submodule is included if its
enclosing module is included and the submodule contains a
sub-submodule named @racketidfont{declare-preserve-for-embedding}
(where the implementation of the sub-submodule is ignored).

Language reader modules that are used only via @hash-lang[] are also
not automatically embedded. To support dynamic use of @hash-lang[]
with a language specification, supply the @DPFlag{lang} flag to
@exec{raco exe}. The argument after @DPFlag{lang} can be a language
name, but more generally it can be text to appear just after
@hash-lang[]. For example, @litchar{at-exp racket/base} makes sense as
an argument to @DPFlag{lang} to allow @racketmodname[at-exp] combined
with @racketmodname[racket/base] as a language for dynamically loaded
modules.

Modules that are implemented directly by extensions---i.e., extensions
that are automatically loaded from @racket[(build-path "compiled"
"native" (system-library-subpath))] to satisfy a
@racket[require]---are treated like other run-time files: a generated
executable uses them from their original location, and they are copied
and packaged together when creating a distribution.

When a module is embedded in an executable, it gets a symbolic name
instead of its original filesystem-based name. The module-name
resolver is configured in the embedding executable to map
collection-based module paths to the embedded symbolic name, but no
such mapping is created for filesystem paths. By default, a module's
symbolic name is generated in an unspecified but deterministic
way where the name starts with @as-index{@litchar{#%embedded:}},
except that the main module is prefixed with @litchar{#%mzc:}. The
relative lack of specification for module names can be be a problem
for language constructs that are sensitive to module names, such as
serialization. To take more control over a module's symbolic name, use
the @DPFlag{named-lib} or @DPFlag{named-file} argument to specify a
prefix that is appended before the module's base name to generate a
symbolic name.

The @exec{raco exe} command works only with module-based programs. The
@racketmodname[compiler/embed] library provides a more general
interface to the embedding mechanism.

A stand-alone executable is ``stand-alone'' in the sense that you can
run it without starting @exec{racket}, @exec{gracket}, or DrRacket.
However, the executable may depend on Racket shared libraries and
possibly other run-time files declared via
@racket[define-runtime-path]. Using @DFlag{embed-dlls} on Windows or
@DFlag{orig-exe} on Unix may produce an executable that is more
stand-alone than otherwise. Options used when building Racket itself
affect the degree to which executables are
stand-alone.@margin-note*{Then standard distribution uses options that
make executables as stand-alone as possible. For a Unix build,
configuring with @DFlag{enable-shared} makes executables less
stand-alone. For a Mac OS build, configuring without
@DFlag{enable-embedfw} makes non-GUI executables less stand-alone.} In
any case, the executable can be packaged with support libraries to
create a self-contained distribution using @exec{raco distribute}, as
described in @secref["exe-dist"].

The @exec{raco exe} command accepts the following command-line flags:

@itemlist[

 @item{@Flag{o} @nonterm{file} --- create the executable as
   @nonterm{file}, adding a suffix to @nonterm{file} as appropriate
   for the platform and executable type. On Mac OS in @DFlag{gui}
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

 @item{@DFlag{embed-dlls} --- On Windows, for a stand-alone executable,
   copies any needed DLLs into the executable. Embedding DLLs makes
   the resulting executable truly stand-alone if it does not depend on
   other external files. Not all DLLs work with embedding, and
   limitations are mostly related to thread-local storage and
   resources, but all DLLs within the main Racket distribution work
   with @DFlag{embed-dlls}.}

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
   initialized as @racket[null] when the executable starts. Beware that
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

 @item{@DFlag{icns} @nonterm{.icns-path} --- on Mac OS, set the icons
   for the generated executable to be the content of
   @nonterm{.icns-path}.}

 @item{@DFlag{orig-exe} --- on Unix, generate an executable based on
   the original @exec{racket} or @exec{gracket} executable, instead of
   a wrapper executable that redirects to the original. If the
   original executable is statically linked to the Racket runtime
   library, then the resulting executable is similarly stand-alone.
   Beware that if the original executable links to Racket as a shared
   library, however, then @exec{raco distribute} cannot work with
   executables that are created with @DFlag{orig-exe} (because the
   wrapper executable normally takes care of finding the shared
   libraries when the executable is distributed to a different
   machine).}

 @item{@DFlag{cs} --- generate an executable based on the @gtech{CS}
   implementation of Racket, which is the default unless running a
   @exec{raco exe} that is based on the @gtech{BC} implementation.}

 @item{@DFlag{3m} --- generate an executable based on the @gtech{3m}
   variant of Racket, which is the default only when running a @exec{raco
   exe} that is based on the @gtech{3m} variant of the @gtech{BC}
   implementation.}

 @item{@DFlag{cgc} --- generate an executable based on the @gtech{CGC}
   variant of Racket, which is the default only when running a
   @exec{raco exe} that is based on the @gtech{CGC} variant
   of the @gtech{BC} implementation.}

 @item{@DPFlag{aux} @nonterm{file} --- attach information to the
   executable based on @nonterm{file}'s suffix; see
   @racket[extract-aux-from-path] for a list of recognized suffixes
   and meanings, and see @racket[create-embedding-executable]'s use of
   auxiliary association for more specific information about how each
   kind of file is used.}

 @item{@DPFlag{lib} @nonterm{module-path} --- include @nonterm{module-path}
   in the executable, even if it is not referenced by the main program,
   so that it is available via @racket[dynamic-require].}

 @item{@DPFlag{lang} @nonterm{lang} --- include modules needed to load
   modules starting @racket[@#,hash-lang[] @#,nonterm{lang}]
   dynamically. The @nonterm{lang} does not have to be a plain
   language or module name; it might be a more general text sequence,
   such as @litchar{at-exp racket/base} to support language
   constructors like @racketmodname[at-exp].
   The initial @racket[require] for a @racket[module] read as
   @nonterm{lang} must be available though the language reader's
   @racketidfont{get-info} function and the @racket['module-language]
   key; languages implemented with
   @racketmodname[syntax/module-reader] support that key
   automatically.}

 @item{@DPFlag{named-lib} @nonterm{prefix} @nonterm{module-path} ---
   like @DPFlag{lib}, but the embedded module's symbolic name is
   specified to be @nonterm{prefix} appended before the library file's
   base name. Specifying a module's symbolic name can be useful with
   language constructs that depend reflexively on a module name, such
   as a serialization format (where a module name is record so that a
   function can be found later for deserialization).}

 @item{@DPFlag{named-file} @nonterm{prefix} @nonterm{file-path} ---
   include @nonterm{file-path} in the executable, even if it is not
   referenced by the main program, and use @nonterm{prefix} before the
   file's base name as the embedded module's symbolic name. Since the
   embedded module's symbolic name is predictable, the module might be
   accessed at run time via @racket[dynamic-require]. A predictable
   module name can also help with serialized data in the same way as
   @DPFlag{named-lib}.}

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
                                    @racketidfont{declare-preserve-for-embedding}.}
         #:changed "6.90.0.23" @elem{Added @DFlag{embed-dlls}.}
         #:changed "7.0.0.17" @elem{Added @DPFlag{lang}.}
         #:changed "7.3.0.6" @elem{Added @DPFlag{named-lib} and @DPFlag{named-file},
                                   and changed generation of symbolic names for embedded
                                   modules to make it deterministic.}]

@; ----------------------------------------------------------------------

@include-section["exe-api.scrbl"]
@include-section["launcher.scrbl"]
@include-section["exe-dylib-path.scrbl"]

