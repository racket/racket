#lang scribble/doc

@(require scribble/manual
          scribble/bnf
          "common.rkt"
          (for-label scheme/gui
                     compiler/compiler
                     compiler/sig
                     compiler/compiler-unit
                     compiler/option
                     compiler/option-unit
                     compiler/cm
                     dynext/compile-sig
                     dynext/link-sig
                     dynext/file-sig
                     launcher/launcher
                     compiler/module-suffix
                     setup/getinfo))

@title{API for Raw Compilation}

@defmodule[compiler/compiler]{

The @racketmodname[compiler/compiler] library provides the
functionality of @exec{raco make} for compilation to bytecode, but
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
                                 [#:skip-paths skip-paths (listof path-string?) null]
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
compiled, as are all such files within subdirectories; the set of such suffixes
is extensible globally as described in @racket[get-module-suffixes], and
@racket[compile-collection-zos] recognizes suffixes from the @racket['libs] group. However,
any file or directory whose path starts with @racket[skip-path] or an element of @racket[skip-paths] is
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
       directory paths) that are not compiled, in addition to the
       contents of @racket[compile-omit-paths].  Do not use this
       field; it is for backward compatibility.}

 @item{@indexed-racket[scribblings] : A list of pairs, each of which
       starts with a path for documentation source. The sources (and
       the files that they require) are compiled in the same way as
       other module files, unless @racket[skip-docs?] is a true value.}

 @item{@indexed-racket[compile-include-files] : A list of filenames (without
       directory paths) to be compiled, in addition to files that
       are compiled based on the file's extension, being in @racket[scribblings],
       or being @racket[require]d by other compiled files.}

 @item{@racket[module-suffixes] and @racket[doc-module-suffixes] ---
       Used indirectly via @racket[get-module-suffixes].}

]

@history[#:changed "6.3" @elem{Added support for @racket[compile-include-files].}]}


@defproc[(compile-directory-zos [path path-string?]
                                [info ()]
                                [#:verbose verbose? any/c #f]
                                [#:skip-path skip-path (or/c path-string? #f) #f]
                                [#:skip-paths skip-paths (listof path-string?) null]
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

@section[#:tag "module-suffix"]{Recognizing Module Suffixes}

@defmodule[compiler/module-suffix]{The
@racketmodname[compiler/module-suffix] library provides functions for
recognizing file suffixes that correspond to Racket modules for the
purposes of compiling files in a directory, running tests for files in
a directory, and so on. The set of suffixes always includes
@filepath{.rkt}, @filepath{.ss}, and @filepath{.scm}, but it can be
extended globally by @filepath{info.rkt} configuration in collections.}

@history[#:added "6.3"]

@defproc[(get-module-suffixes [#:group group (or/c 'all 'libs 'docs) 'all]
                              [#:mode mode (or/c 'preferred 'all-available 'no-planet 'no-user) 'preferred]
                              [#:namespace namespace (or/c #f namespace?) #f])
         (listof bytes?)]{

Inspects @filepath{info.rkt} files (see @secref["info.rkt"]) of
installed collections to produce a list of file suffixes that should
be recognized as Racket modules. Each suffix is reported as a byte
string that does not include the @litchar{.} that precedes a suffix.

The @racket[mode] and @racket[namespace] arguments are propagated to
@racket[find-relevant-directories] to determine which collection
directories might configure the set of suffixes. Consequently, suffix
registrations are found reliably only if @exec{raco setup} (or package
installations or updates that trigger @exec{raco setup}) is run.

The @racket[group] argument determines whether the result includes all
registered suffixes, only those that are registered as general library
suffixes, or only those that are registered as documentation suffixes.
The set of general-library suffixes always includes @filepath{.rkt},
@filepath{.ss}, and @filepath{.scm}. The set of documentation suffixes
always includes @filepath{.scrbl}.

The following fields in an @filepath{info.rkt} file extend the set of
suffixes:

@itemize[

 @item{@indexed-racket[module-suffixes] : A list of byte strings that
       correspond to general-library module suffixes (without the
       @litchar{.} that must appear before the suffix). Non-lists or
       non-byte-string elements of the list are ignored.}

 @item{@indexed-racket[doc-module-suffixes] : A list of byte strings
       as for @racket[module-suffixes], but for documentation
       modules.}

]}

@defproc[(get-module-suffix-regexp [#:group group (or/c 'all 'libs 'docs) 'all]
                                   [#:mode mode (or/c 'preferred 'all-available 'no-planet 'no-user) 'preferred]
                                   [#:namespace namespace (or/c #f namespace?) #f])
         byte-regexp?]{

Returns a @tech[#:doc reference-doc]{regexp value} that matches paths ending
with a suffix as reported by @racket[get-module-suffixes]. The pattern
includes a subpatterns for the suffix without its leading @litchar{.}}


@; ----------------------------------------------------------------------

@section[#:tag "api:loading"]{Loading Compiler Support}

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

@defparam[compile-subcollections cols (one-of/c #t #f)]{

A parameter that specifies whether sub-collections are compiled by
@racket[compile-collection-zos].  The default is @racket[#t].}


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


@; ----------------------------------------

@subsection{Main Compiler Unit}

@defmodule[compiler/compiler-unit]

@defthing[compiler@ unit?]{

Provides the exports of @racketmodname[compiler/compiler] in unit
form, where C-compiler operations are imports to the unit, although 
they are not used.

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
