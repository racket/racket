#lang scribble/doc

@(require scribble/manual
          scribble/bnf
          (for-label scheme/gui
                     compiler/compiler
                     compiler/sig
                     compiler/compiler-unit
                     compiler/option
                     compiler/option-unit
                     compiler/comp-unit
                     compiler/cm
                     dynext/compile-sig
                     dynext/link-sig
                     dynext/file-sig
                     launcher/launcher))

@title{API for Raw Compilation}

@defmodule[compiler/compiler]{

The @racketmodname[compiler/compiler] library provides the
functionality of @exec{mzc} for compilation to bytecode and via C, but
through a Racket API.}

@; ----------------------------------------------------------------------

@section[#:tag "api:zo"]{Bytecode Compilation}

@defproc[((compile-zos [expr any/c] [#:module? module? any/c #f] [#:verbose? verbose? any/c #f]) 
          [racket-files (listof path-string?)]
          [dest-dir (or/c path-string? false/c (one-of/c 'auto))])
         void?]{

Supplying just @racket[expr] returns a compiler that is initialized
with the expression @racket[expr], as described below.

The compiler takes a list of Racket files and compiles each of them to
bytecode, placing the resulting bytecode in a @filepath{.zo} file
within the directory specified by @racket[dest-dir].  If
@racket[dest-dir] is @racket[#f], each bytecode result is placed in
the same directory as its source file.  If @racket[dest-dir] is
@racket['auto], each bytecode file is placed in a @filepath{compiled}
subdirectory relative to the source; the directory is created if
necessary.

If @racket[expr] is anything other than @racket[#f], then a namespace
is created for compiling the files that are supplied later, and
@racket[expr] is evaluated to initialize the created namespace. For
example, @racket[expr] might load a set of macros. In addition, the
expansion-time part of each expression later compiled is evaluated in
the namespace before being compiled, so that the effects are visible
when compiling later expressions.

If @racket[expr] is @racket[#f], then no compilation namespace is
created (the current namespace is used), and expressions in the files
are assumed to compile independently (so there's no need to evaluate
the expansion-time part of an expression to compile).

Typically, @racket[expr] is @racket[#f] for compiling @racket[module]
files, and it is @racket[(void)] for compiling files with top-level
definitions and expressions.

If @racket[module?] is @racket[#t], then the given files are read and
compiled as modules (so there is no dependency on the current
namespace's top-level environment).

If @racket[verbose?] is @racket[#t], the output file for each given
file is reported through the current output port.}


@defproc[(compile-collection-zos [collection string?] ...+
                                 [#:skip-path skip-path (or/c path-string? #f) #f]
                                 [#:skip-doc-sources? skip-docs? any/c #f]
                                 [#:managed-compile-zo managed-compile-zo 
                                                       (path-string? . -> . void?)
                                                       (make-caching-managed-compile-zo)])
         void?]{

Compiles the specified collection's files to @filepath{.zo} files
by using @racket[managed-compile-zo] on each source file.
The @filepath{.zo} files are placed into the collection's
@filepath{compiled} directory.

By default, all files with the
extension @filepath{.rkt}, @filepath{.ss}, or @filepath{.scm} in a collection are
compiled, as are all such files within subdirectories, execept that
any file or directory whose path starts with @racket[skip-path] is
skipped. (``Starts with'' means that the simplified path @racket[_p]'s
byte-string form after @racket[(simplify-path _p #f)]starts with the
byte-string form of @racket[(simplify-path skip-path #f)].)

The collection compiler reads the collection's @filepath{info.rkt} file
(see @secref["info.rkt"]) to obtain further instructions for compiling the
collection.  The following fields are used:

@itemize[

 @item{@indexed-racket[name] : The name of the collection as a string, used
       only for status and error reporting.}

 @item{@indexed-racket[compile-omit-paths] : A list of immediate file
       and directory paths that should not be compiled. Alternatively,
       this field's value @racket['all], which is equivalent to
       specifying all files and directories in the collection (to
       effectively ignore the collection for
       compilation). Automatically omitted files and directories are
       @filepath{compiled}, @filepath{doc}, and those whose names
       start with @litchar{.}.

       Files that are required by other files, however, are always
       compiled in the process of compiling the requiring file---even
       when the required file is listed with this field or when the
       field's value is @racket['all].}

 @item{@indexed-racket[compile-omit-files] : A list of filenames (without
       directory paths); that are not compiled, in addition to the
       contents of @racket[compile-omit-paths].  Do not use this
       field; it is for backward compatibility.}

 @item{@indexed-racket[scribblings] : A list of pairs, each of which
       starts with a path for documentation source. The sources (and
       the files that they require) are compiled in the same way as
       @filepath{.rkt}, @filepath{.ss}, and @filepath{.scm} files,
       unless the provided @racket[skip-docs?] argument is a true
       value.}

]}


@defproc[(compile-directory-zos [path path-string?]
                                [info ()]
                                [#:verbose verbose? any/c #f]
                                [#:skip-path skip-path (or/c path-string? #f) #f]
                                [#:skip-doc-sources? skip-docs? any/c #f]
                                [#:managed-compile-zo managed-compile-zo 
                                                      (path-string? . -> . void?)
                                                      (make-caching-managed-compile-zo)])
         void?]{

Like @racket[compile-collection-zos], but compiles the given directory
rather than a collection. The @racket[info] function behaves like the
result of @racket[get-info] to supply @filepath{info.rkt} fields,
instead of using an @filepath{info.rkt} file (if any) in the directory.}

@; ----------------------------------------------------------------------

@section[#:tag "api:ext"]{Compilation via C}

@defproc[((compile-extensions [expr any/c]) 
          [racket-files (listof path-string?)]
          [dest-dir (or/c path-string? false/c (one-of/c 'auto))])
         void?]{

Like @racket[compile-zos], but the @racket[racket-files] are compiled
to native-code extensions via C. If @racket[dest-dir] is
@racket['auto], each extension file (@filepath{.dll}, @filepath{.so},
or @filepath{.dylib}) is placed in a subdirectory relative to the
source produced by @racket[(build-path "compiled" "native"
(system-library-subpath))]; the directory is created if necessary.}


@defproc[((compile-extensions-to-c [expr any/c]) 
          [racket-files (listof path-string?)]
          [dest-dir (or/c path-string? false/c (one-of/c 'auto))])
         void?]{

Like @racket[compile-extensions], but only @filepath{.c} files are
produced, not extensions.}


@defproc[(compile-c-extensions
          [c-files (listof path-string?)]
          [dest-dir (or/c path-string? false/c (one-of/c 'auto))])
         void?]{

Compiles each @filepath{.c} file (usually produced with
@racket[compile-extensions-to-c]) in @racket[c-files] to an
extension. The @racket[dest-dir] argument is handled as in
@racket[compile-extensions]. }

@; ----------------------------------------------------------------------

@section[#:tag "api:loading"]{Loading compiler support}

The compiler unit loads certain tools on demand via @racket[dynamic-require]
and @racket[get-info]. If the namespace used during compilation is different
from the namespace used to load the compiler, or if other load-related
parameters are set, then the following parameter can be used to
restore settings for @racket[dynamic-require].

@defparam[current-compiler-dynamic-require-wrapper
          proc 
          ((-> any) . -> . any)]{

A parameter whose value is a procedure that takes a thunk to
apply. The default wrapper sets the current namespace (via
@racket[parameterize]) before calling the thunk, using the namespace
in which the @racket[compiler/compiler] library was originally
instantiated.}

@; ----------------------------------------------------------------------

@section[#:tag "api:options"]{Options for the Compiler}

@defmodule[compiler/option]{

The @racketmodname[compiler/option] module provides options (in the
form of parameters) that control the compiler's behaviors.}

More options are defined by the @racketmodname[dynext/compile] and
@racketmodname[dynext/link] libraries, which control the actual C
compiler and linker that are used for compilation via C.

@defboolparam[somewhat-verbose on?]{

A @racket[#t] value for the parameter causes the compiler to print
the files that it compiles and produces. The default is @racket[#f].}

@defboolparam[verbose on?]{

A @racket[#t] value for the parameter causes the compiler to print
verbose messages about its operations. The default is @racket[#f].}

@defparam[setup-prefix str string?]{

A parameter that specifies a string to embed in public function names
when compiling via C.  This is used mainly for compiling extensions
with the collection name so that cross-extension conflicts are less
likely in architectures that expose the public names of loaded
extensions. The default is @racket[""].}

@defboolparam[clean-intermediate-files clean?]{

A @racket[#f] value for the parameter keeps intermediate @filepath{.c}
and @filepath{.o} files generated during compilation via C. The
default is @racket[#t].}

@defparam[compile-subcollections cols (one-of/c #t #f)]{

A parameter that specifies whether sub-collections are compiled by
@racket[compile-collection-zos].  The default is @racket[#t].}


@defboolparam[compile-for-embedded embed?]{

A @racket[#t] values for this parameter creates @filepath{.c} files
and object files to be linked directly with an embedded Racket
run-time system, instead of @filepath{.c} files and object files to be
dynamically loaded into Racket as an extension. The default is
@racket[#f].}


@defboolparam[propagate-constants prop?]{

A parameter to control the compiler's constant propagating when
compiling via C. The default is @racket[#t].}


@defboolparam[assume-primitives assume?]{

A @racket[#t] parameter value effectively adds @racket[(require
mzscheme)] to the beginning of the program. This parameter is useful
only when compiling non-@racket[module] code.  The default is
@racket[#f].}

@defboolparam[stupid allow?]{

A parameter that allow obvious non-syntactic errors, such as
@racket[((lambda () 0) 1 2 3)], when compiling via C. The default is
@racket[#f].}

@defparam[vehicles mode symbol?]{

A parameter that controls how closures are compiled via C.  The
possible values are: 

@itemize[

 @item{@racket['vehicles:automatic] : automatic grouping}

 @item{@racket['vehicles:functions] : groups within a procedure}

 @item{@racket['vehicles:monolithic] : groups randomly}

]}

@defparam[vehicles:monoliths count exact-nonnegative-integer?]{

A parameter that determines the number of random
groups for @racket['vehicles:monolithic] mode.}

@defparam[seed val exact-nonnegative-integer?]{

Sets the randomizer seed for @racket['vehicles:monolithic] mode.}

@defparam[max-exprs-per-top-level-set n exact-nonnegative-integer?]{

A parameter that determines the number of top-level Racket expressions
crammed into one C function when compiling via C.  The default is
@racket[25].}

@defboolparam[unpack-environments unpack?]{

Setting this parameter to @racket[#f] might help compilation via C
for register-poor architectures.  The default is @racket[#t].}

@defboolparam[debug on?]{

A @racket[#t] creates a @filepath{debug.txt} debugging file when
compiling via C.  The default is @racket[#f].}

@defboolparam[test on?]{

A @racket[#t] value for this parameter causes compilation via C to
ignore top-level expressions with syntax errors. The default is
@racket[#f].}

@; ----------------------------------------------------------------------

@section[#:tag "api:unit"]{The Compiler as a Unit}

@; ----------------------------------------

@subsection{Signatures}

@defmodule[compiler/sig]

@defsignature/splice[compiler^ ()]{

Includes all of the names exported by
@racketmodname[compiler/compiler].}

@defsignature/splice[compiler:option^ ()]{

Includes all of the names exported by
@racketmodname[compiler/option].}


@defsignature[compiler:inner^ ()]{

@signature-desc{The high-level @racketmodname[compiler/compiler]
interface relies on a low-level implementation of the extension
compiler, which is available from @racketmodname[compiler/comp-unit]
as implementing the @racket[compiler:inner^] signature.}

@defproc[(eval-compile-prefix [expr any/c]) void?]{

Evaluates @racket[expr]. Future calls to @sigelem[compiler:inner^
compile-extension] or @sigelem[compiler:inner^ compile-extension-to-c]
see the effects of the evaluation.}

@defproc[(compile-extension [racket-source path-string?] 
                            [dest-dir path-string?])
         void?]{

Compiles a single Racket file to an extension.}

@defproc[(compile-extension-to-c [racket-source path-string?] 
                                 [dest-dir path-string?])
         void?]{

Compiles a single Racket file to a @filepath{.c} file.}

@defproc[(compile-c-extension [c-source path-string?] 
                              [dest-dir path-string?])
         void?]{

Compiles a single @filepath{.c} file to an extension.}

}

@; ----------------------------------------

@subsection{Main Compiler Unit}

@defmodule[compiler/compiler-unit]

@defthing[compiler@ unit?]{

Provides the exports of @racketmodname[compiler/compiler] in unit
form, where C-compiler operations are imports to the unit.

The unit imports @racket[compiler:option^], @racket[dynext:compile^],
@racket[dynext:link^], and @racket[dynext:file^]. It exports
@racket[compiler^].}

@; ----------------------------------------

@subsection{Options Unit}

@defmodule[compiler/option-unit]

@defthing[compiler:option@ unit?]{

Provides the exports of @racketmodname[compiler/option] in unit
form. It imports no signatures, and exports
@racket[compiler:option^].}

@; ----------------------------------------

@subsection{Compiler Inner Unit}

@defmodule[compiler/comp-unit]

@defthing[comp@ unit?]{

The unit imports @racket[compiler:option^], @racket[dynext:compile^],
@racket[dynext:link^], and @racket[dynext:file^]. It exports
@racket[compiler:inner^].}

