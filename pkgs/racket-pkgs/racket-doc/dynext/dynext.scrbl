#lang scribble/doc

@(require
  scribble/manual
  (for-label scheme
             dynext
             dynext/compile-unit
             dynext/compile-sig
             dynext/link-unit
             dynext/link-sig
             dynext/file-unit
             dynext/file-sig))

@title{Dynext: Running a C Compiler/Linker}

The @filepath{dynext} collection provides libraries for using a
platform-specific C compiler and linker.

@table-of-contents[]

@; ----------------------------------------------------------------------

@section{Compilation}

@defmodule[dynext/compile]

@defproc[(compile-extension 
          (quiet? any/c) 
          (input-file path-string?)
          (output-file path-string?)
          (include-dirs (listof path-string?)))
         any/c]{

Compiles the given input file (C source) to the given output file (a
compiled-object file). The @racket[quiet?] argument indicates whether
command should be echoed to the current output port. The
@racket[include-dirs] argument is a list of directories to search for
include files; the Racket installation's @filepath{include}
directories are added automatically.}


@subsection[#:tag "compile-params"]{Compilation Parameters}

@defparam[current-extension-compiler 
          compiler
          (or/c path-string? false/c)]{

A parameter that determines the executable for the compiler. 

The default is set by searching for an executable using the
@envvar{PATH} environment variable, or using the @envvar{CC} or
@envvar{MZSCHEME_DYNEXT_COMPILER} environment variable if either is
defined (and the latter takes precedence). On Windows, the search
looks for @filepath{cl.exe}, then @filepath{gcc.exe}, then
@filepath{bcc32.exe} (Borland). On Unix, it looks for
@filepath{gcc}, then @filepath{cc}. A @racket[#f] value indicates that
no compiler could be found.}

@defparam[current-extension-compiler-flags
          flags
          (listof (or/c path-string? 
                        (-> (or/c null? (listof string?)))))]{

A parameter that determines strings passed to the compiler as flags.
See also @racket[expand-for-compile-variant].

On Windows, the default is @racket[(list "/c" "/O2" "/MT"
_3m-flag-thunk)] for @filepath{cl.exe}, or @racket[(list "-c" "-O2"
"-fPIC" _3m-flag-thunk)] for @filepath{gcc.exe} and
@filepath{bcc32.exe}, where @racket[_3m-flag-thunk] returns
@racket[(list "-DMZ_PRECISE_GC")] for the 3m variant and null for the
CGC variant. On Unix, the default is usually @racket[(list "-c"
"-O2" "-fPIC" _3m-flag-thunk)]. If the @envvar{CFLAGS} or
@envvar{MZSCHEME_DYNEXT_COMPILER_FLAGS} environment variable is
defined (the latter takes precedence), then its value is parsed as a
list of strings that is appended before the defaults.}


@defparam[current-make-compile-include-strings
          proc
          (-> path-string? (listof string?))]{

A parameter the processes include-path inputs to the compiler; the
parameter values takes an include directory path and returns a list of
strings for the command line.

On Windows, the default converts @racket["dir"] to @racket[(list
"/Idir")] for @filepath{cl.exe}, @racket[(list "-Idir")] for
@filepath{gcc.exe} and @filepath{bcc32.exe}. On Unix, the default
converts @racket["dir"] to @racket[(list "-Idir")]. If the
@envvar{CFLAGS} environment variable is defined, then its value is
parsed as a list of flags that is appended before the defaults.}


@defparam[current-make-compile-input-strings
          proc
          (-> (or/c string? path?) (listof string?))]{

A parameter that processes inputs to the compiler; the parameter's
values takes an input file path and returns a list of strings for the
command line.  The default is @racket[list].}


@defparam[current-make-compile-output-strings
          proc
          (-> (or/c string? path?) (listof string?))]{

A parameter that processes outputs specified for the compiler; the
parameter's value takes an output file path and returns a list of
strings for the command line.

On Windows, the default converts @racket["file"] to @racket[(list
"/Fofile")] for @filepath{cl.exe}, or to @racket[(list "-o" "file")]
for @filepath{gcc.exe} and @filepath{bcc32.exe}. On Unix, the
default converts @racket["file"] to @racket[(list "-o" "file")].}


@defparam[current-extension-preprocess-flags
          flags
          (listof (or/c string? path? (-> (or/c string? path?))))]{

A parameters that specifies flags to the compiler preprocessor,
instead of to the compiler proper; use these flags for preprocessing
instead of @racket[current-extension-compiler-flags].

The defaults are similar to @racket[current-extension-compiler-flags],
but with @racket["/E"] (Windows @filepath{cl.exe}) or @racket["-E"]
and without non-@racket["-D"] flags.}


@defparam[compile-variant
          variant-symbol
          (one-of/c 'normal 'cgc '3m)]{

A parameter that indicates the target for compilation, where
@racket['normal] is an alias for the result of @racket[(system-type
'gc)]}


@subsection{Helper functions}

@defproc[(use-standard-compiler (name (apply one-of/c (get-standard-compilers)))) any]{

Sets the parameters described in @secref["compile-params"] for a
particular known compiler. The acceptable names are
platforms-specific:

@itemize[
@item{Unix: @racket['cc] or @racket['gcc]}
@item{Windows: @racket['gcc], @racket['msvc], or @racket['borland]}
@item{MacOS: @racket['cw]}
]}


@defproc[(get-standard-compilers) (listof symbol?)]{

Returns a list of standard compiler names for the current platform. See
@racket[use-standard-compiler].}


@defproc[(expand-for-compile-variant (l (listof (or/c path-string? (-> (listof string?)))))) any]{

Takes a list of paths and thunks and returns a list of strings. Each
thunk in the input list is applied to get a list of strings that is
inlined in the corresponding position in the output list. This
expansion enables occasional parametrization of flag lists, etc.,
depending on the current compile variant.}

@subsection[#:tag "compile-sig"]{Signature}

@defmodule[dynext/compile-sig]

@defsignature[dynext:compile^ ()]

Includes everything exported by the @racketmodname[dynext/compile] module.

@subsection[#:tag "compile-unit"]{Unit}

@defmodule[dynext/compile-unit]

@defthing[dynext:compile@ unit?]{

Imports nothing, exports @racket[dynext:compile^].}

@; ----------------------------------------------------------------------

@section{Linking}

@defmodule[dynext/link]

@defproc[(link-extension (quiet? any/c) (input-files (listof path-string?)) (output-file path-string?)) any]{

Links object files to create an extension (normally of a form that can
be loaded with @racket[load-extension]).

The @racket[quiet?] argument indicates whether command should be
echoed to the current output port. The @racket[input-files] argument
is list of compiled object filenames, and @racket[output-file] is the
destination extension filename.}


@subsection[#:tag "link-params"]{Linking Parameters}

@defparam[current-extension-linker
          linker
          (or/c path-string? false/c)]{

A parameter that determines the executable used as a linker.

The default is set by searching for an executable using the
@envvar{PATH} environment variable, or by using the @envvar{LD} or
@envvar{MZSCHEME_DYNEXT_LINKER} environment variable if it is defined
(and the latter takes precedence). On Windows, it looks for
@filepath{cl.exe}, then @filepath{ld.exe} (gcc), then
@filepath{ilink32.exe} (Borland). On Cygwin, Solaris, FreeBSD 2.x,
or HP/UX, it looks for @filepath{ld}. On other Unix variants, it
looks for @filepath{cc}. @racket[#f] indicates that no linker could be
found.}

@defparam[current-extension-linker-flags
          flags
          (listof (or/c path-string? (-> (listof string?))))]{

A parameter that determines flags provided to the linker. See also
@racket[expand-for-link-variant].

On Windows, default is @racket[(list "/LD")] for @filepath{cl.exe},
@racket[(list "--dll")] for @filepath{ld.exe}, and @racket[(list
"/Tpd" "/c")] for @filepath{ilink32.exe}.  On Unix, the default
varies greatly among platforms.  If the @envvar{LDFLAGS} or
@envvar{MZSCHEME_DYNEXT_LINKER_FLAGS} (the latter takes precedence)
environment variable is defined, then its value is parsed as a list of
strings that is appended before the defaults.}

@defparam[current-make-link-input-strings
          proc
          (-> path-string? (listof string?))]{

A parameter that processes linker input arguments; the parameter value
takes an input file path and returns a list of strings for the command
line.  The default is @racket[list].}

@defparam[current-make-link-output-strings
          proc
          (-> path-string? (listof string?))]{

A parameter that processes linker output arguments; the parameter
value takes an output file path and returns a list of strings for the
command line.

On Windows, the default converts @racket["file"] to @racket[(list
"/Fefile")] for @filepath{cl.exe}, something like @racket[(list "-e"
"_dll_entry@12" "-o" "file")] for @filepath{ld.exe}, and something
complex for @filepath{ilink32.exe}. On Unix, the default converts
@racket["file"] to @racket[(list "-o" "file")].}

@defparam[current-standard-link-libraries
          libs
          (listof (or/c path-string? (-> (listof string?))))]{

A parameter that determines libraries supplied to the linker, in
addition to other inputs.  See also @racket[expand-for-link-variant].

For most platforms, the default is

@racketblock[(list (build-path (collection-path "mzscheme" "lib") 
                               (system-library-subpath)
                               (_mzdyn-thunk)))]

where @racket[_mzdyn-thunk] produces @racket[(list "mzdyn.o")] for the
@racket['cgc] variant and @racket[(list "mzdyn3m.o")] for the
@racket['3m] variant. See also @racket[current-use-mzdyn]).}

@defparam[current-use-mzdyn
  use-mzdyn?
  boolean?]{

A parameter that determines whether the default standard link
libraries include the @filepath{mzdyn} library which allows the
resulting file to be loaded via @racket[load-extension].  Defaults to
@racket[#t].}

@defparam[link-variant
          variant-symbol
          (one-of/c 'normal 'cgc '3m)]{

A parameter that indicates the target for linking, where
@racket['normal] is an alias for the result of @racket[(system-type
'gc)].}


@subsection{Helper Functions}

@defproc[(use-standard-linker (name (one-of/c 'cc 'gcc 'msvc 'borland 'cw)))
         void?]{

Sets the parameters described in @secref["link-params"] for a
particular known linker.}


@defproc[(expand-for-link-variant (l (listof (or/c path? 
                                                   string? 
                                                   (-> (listof string?)))))) 
         any]{

The same as @racket[expand-for-compile-variant].}


@subsection[#:tag "link-sig"]{Signature}

@defmodule[dynext/link-sig]

@defsignature[dynext:link^ ()]

Includes everything exported by the @racketmodname[dynext/link] module.

@subsection[#:tag "link-unit"]{Unit}

@defmodule[dynext/link-unit]

@defthing[dynext:link@ unit?]{

Imports nothing, exports @racket[dynext:link^].}

@; ----------------------------------------------------------------------

@section{Filenames}

@defmodule[dynext/file]

@defproc[(append-zo-suffix (s (or/c string? path?))) path?]{

Appends the @filepath{.zo} file suffix to @racket[s], returning a
path. The existing suffix, if any, is preserved and converted as with
@racket[path-add-suffix].}

@defproc[(append-object-suffix (s path-string?)) path?]{

Appends the platform-standard compiled object file suffix to
@racket[s], returning a path.}

@defproc[(append-c-suffix (s path-string?)) path?]{

Appends the platform-standard C source-file suffix to @racket[s],
returning a path.}

@defproc[(append-constant-pool-suffix (s (or/c string? path?))) path?]{

Appends the constant-pool file suffix @filepath{.kp} to @racket[s],
returning a path.}

@defproc[(append-extension-suffix (s (or/c string? path?))) path?]{

Appends the platform-standard dynamic-extension file suffix to
@racket[s], returning a path.}

@defproc[(extract-base-filename/ss (s path-string?) 
                                   (program any/c #f))
         (or/c path? false/c)]{

Strips the Racket file suffix from @racket[s] and returns a stripped
path. Unlike the other functions below, when @racket[program] is not
@racket[#f], then any suffix (including no suffix) is allowed. If
@racket[s] is not a Racket file and @racket[program] is @racket[#f],
@racket[#f] is returned.}

@defproc[(extract-base-filename/c (s path-string?) 
                                  (program any/c #f)) 
         (or/c path? false/c)]{

Strips the Racket file suffix from @racket[s] and
returns a stripped path. If @racket[s] is not a Racket file name and
@racket[program] is a symbol, and error is signaled.  If @racket[s] is
not a Racket file and @racket[program] is @racket[#f], @racket[#f] is
returned.}

@defproc[(extract-base-filename/kp (s path-string?) (program any/c #f)) (or/c path? false/c)]{

Same as @racket[extract-base-filename/c], but for constant-pool
files.}

@defproc[(extract-base-filename/o (s path-string?) (program any/c #f)) (or/c path? false/c)]{

Same as @racket[extract-base-filename/c], but for compiled-object
files.}

@defproc[(extract-base-filename/ext (s path-string?) (program any/c #f)) (or/c path? false/c)]{

Same as @racket[extract-base-filename/c], but for extension files.}


@subsection[#:tag "file-sig"]{Signature}

@defmodule[dynext/file-sig]

@defsignature[dynext:file^ ()]

Includes everything exported by the @racketmodname[dynext/file] module.

@subsection[#:tag "file-unit"]{Unit}

@defmodule[dynext/file-unit]

@defthing[dynext:file@ unit?]{

Imports nothing, exports @racket[dynext:file^].}

