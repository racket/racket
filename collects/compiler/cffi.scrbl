#lang scribble/doc
@(require scribble/manual
          scribble/struct
          (for-label scheme/base
                     compiler/cffi
                     mzlib/include))

@(define ctool (exec "raco ctool"))
@(define cpp tt)
@(define inside @other-manual['(lib "scribblings/inside/inside.scrbl")])

@(define (lines . l)
   (make-table #f
               (map (lambda (l)
                      (list
                       (make-flow
                        (list
                         (if (table? l)
                             l
                             (make-paragraph (list l)))))))
                    l)))

@(define (argtype scm desc c . cases)
   (item (apply lines 
                scm 
                @elem{Racket range: @|desc|}
                @elem{C type: @|c|}
                cases)))

@(define (toC . content)
   (apply elem "Racket to C conversion: " content))
@(define (toRacket . content)
   (apply elem "C to Racket conversion: " content))
@(define tosObvious
   (elem "conversions: (obvious and precise)"))
@(define goesto 'rarr)

   

@title{@scheme[c-lambda]: C FFI via @exec{raco ctool}}

@defmodule[compiler/cffi]{

The @schememodname[compiler/cffi] module relies on a C compiler to
statically construct an interface to C code through directives
embedded in a Racket program. The library implements a subset of
@as-index{Gambit-C}'s foreign-function interface @cite["Feeley98"].}

The @racketmodname[ffi/unsafe] library is a better interface for
most tasks; see @other-manual['(lib
"scribblings/foreign/foreign.scrbl")] for more information on
@racketmodname[ffi/unsafe]. See also @|inside|, which describes
Racket's C-level API for extending the run-time system.

The @racketmodname[compiler/cffi] library defines three forms:
@racket[c-lambda], @racket[c-declare], and @racket[c-include]. When
interpreted directly or compiled to byte code, @racket[c-lambda]
produces a function that always raises @racket[exn:fail], and
@racket[c-declare] and @racket[c-include] raise
@racket[exn:fail]. When compiled by @exec{raco ctool --extension}, the forms
provide access to C. Thus, @racketmodname[compiler/cffi] is normally
required by a module to be compiled via @|ctool|. In addition, the
@|ctool| compiler implicitly imports @racketmodname[compiler/cffi] into
the top-level environment for non-@racket[module] compilation.

The @racket[c-lambda] form creates a Racket procedure whose body is
implemented in C. Instead of declaring argument names, a
@racket[c-lambda] form declares argument types, as well as a return
type. The implementation can be simply the name of a C function, as in
the following definition of @racket[fmod]:

@racketblock[
(define fmod (c-lambda (double double) double "fmod"))
]

Alternatively, the implementation can be C code to serve as the body
of a function, where the arguments are bound to @cpp{___arg1} (three
underscores), etc., and the result is installed into @cpp{___result}
(three underscores):

@racketblock[
(define machine-string->float
  (c-lambda (char-string) float
     "___result = *(float *)___arg1;"))
]

The @racket[c-lambda] form provides only limited conversions between C
and Racket data. For example, the following function does not reliably
produce a string of four characters:

@racketblock[
(define broken-machine-float->string
  (c-lambda (float) char-string
     "char b[5]; *(float *)b = ___arg1; b[4] = 0; ___result = b;"))
]

because the representation of a @cpp{float} can contain null bytes,
which terminate the string. However, the full Racket API, which is
described in @|inside|, can be used in a function body:

@racketblock[
(define machine-float->string
  (c-lambda (float) racket-object
     "char b[4];"
     "*(float *)b = ___arg1;"
     "___result = racket_make_sized_byte_string(b, 4, 1);"))
]

The @racket[c-declare] form declares arbitrary C code to appear after
@filepath{escheme.h} or @filepath{scheme.h} is included, but before
any other code in the compilation environment of the declaration. It
is often used to declare C header file inclusions. For example, a
proper definition of @racket[fmod] needs the @filepath{math.h} header
file:

@racketblock[
(c-declare "#include <math.h>")
(define fmod (c-lambda (double double) double "fmod"))
]

The @racket[c-declare] form can also be used to define helper C
functions to be called through @racket[c-lambda].

The @racket[c-include] form expands to a @racket[c-declare] form using
the content of a specified file. Use @racket[(c-include _file)] instead
of @racket[(c-declare "#include file")] when it's easier to
have Racket resolve the file path than to have the C compiler
resolve it.

The @filepath{collects/mzscheme/examples} directory in the Racket
distribution contains additional examples.

When compiling for Racket 3m (see @|inside|), C code inserted by
@racket[c-lambda], @racket[c-declare], and @racket[c-include] will be
transformed in the same was as @|ctool|'s @DFlag{xform} mode (which may
or may not be enough to make the code work correctly in Racket 3m;
see @|inside| for more information).


@defform[(c-lambda (argument-type ...) return-type impl-string ...+)]{

Creates a Racket procedure whose body is implemented in C. The
procedure takes as many arguments as the supplied
@racket[argument-type]s, and it returns one value. If
@racket[return-type] is @racketidfont{void}, the procedure's result is
always void.  The @racket[impl-string] is either the name of a C
function (or macro) or the body of a C function.

If a single @racket[impl-string] is provided, and if it is a
string containing only alphanumeric characters and @litchar{_},
then the created Racket procedure passes all of its arguments to
the named C function (or macro) and returns the function's
result. Each argument to the Racket procedure is converted
according to the corresponding @racket[argument-type] (as
described below) to produce an argument to the C function. Unless
@racket[return-type] is @racketidfont{void}, the C function's result is
converted according to @racket[return-type] for the Racket
procedure's result.

If more than @racket[impl-string] is provided, or if it contains more
than alphanumeric characters and @litchar{_}, then the concatenated
@racket[impl-string]s must contain C code to implement the function
body. The converted arguments for the function will be in variables
@cpp{___arg1}, @cpp{___arg2}, ... (with three underscores in each
name) in the context where the @racket[impl-string]s are placed for
compilation. Unless @racket[return-type] is @racketidfont{void}, the
@racket[impl-string]s code should assign a result to the variable
@cpp{___result} (three underscores), which will be declared but not
initialized. The @racket[impl-string]s code should not return
explicitly; control should always reach the end of the body. If the
@racket[impl-string]s code defines the pre-processor macro
@cpp{___AT_END} (with three leading underscores), then the macro's
value should be C code to execute after the value @cpp{___result} is
converted to a Racket result, but before the result is returned, all
in the same block; defining @cpp{___AT_END} is primarily useful for
deallocating a string in @cpp{___result} that has been copied by
conversion.  The @racket[impl-string]s code will start on a new line at
the beginning of a block in its compilation context, and
@cpp{___AT_END} will be undefined after the code.

In addition to @cpp{___arg1}, etc., the variable @cpp{argc} is bound
in @racket[impl-string]s to the number of arguments supplied to
the function, and @cpp{argv} is bound to a @cpp{Racket_Object*} array
of length @cpp{argc} containing the function arguments as Racket
values. The @cpp{argv} and @cpp{argc} variables are mainly useful for
error reporting (e.g., with @cpp{racket_wrong_type}).

Each @racket[argument-type] must be one of the following, which are
recognized symbolically:

@itemize[

  @argtype[@racket[bool] "any value" @cpp{int}
           @toC{@racket[#f] @|goesto| 0, anything else @|goesto| 1}
           @toRacket{0 @|goesto| @racket[#f], anything else @|goesto| @racket[#t]}]

  @argtype[@racket[char] "character" @cpp{char}
           @toC{character's Latin-1 value cast to signed byte}
           @toRacket{Latin-1 value from unsigned cast mapped to character}]

  @argtype[@racket[unsigned-char] "character" @cpp{unsigned char}
           @toC{character's Latin-1 value}
           @toRacket{Latin-1 value mapped to character}]

  @argtype[@racket[signed-char] "character" @cpp{signed char}
           @toC{character's Latin-1 value cast to signed byte}
           @toRacket{Latin-1 value from unsigned cast mapped to character}]

  @argtype[@racket[int] @elem{exact integer that fits into an @cpp{int}} @cpp{int}
           @tosObvious]

  @argtype[@racket[unsigned-int] @elem{exact integer that fits into an @cpp{unsigned int}} @cpp{unsigned int}
           @tosObvious]

  @argtype[@racket[long] @elem{exact integer that fits into a @cpp{long}} @cpp{long}
           @tosObvious]

  @argtype[@racket[unsigned-long] @elem{exact integer that fits into an @cpp{unsigned long}} @cpp{unsigned long}
           @tosObvious]

  @argtype[@racket[short] @elem{exact integer that fits into a @cpp{short}} @cpp{short}
           @tosObvious]

  @argtype[@racket[unsigned-short] @elem{exact integer that fits into an @cpp{unsigned short}} @cpp{unsigned short}
           @tosObvious]

  @argtype[@racket[float] "real number" @cpp{float}
           @toC{number converted to inexact and cast to @cpp{float}}
           @toRacket{cast to @cpp{double} and encapsulated as an inexact number}]

  @argtype[@racket[double] "real number" @cpp{double}
           @toC{number converted to inexact}
           @toRacket{encapsulated as an inexact number}]

  @argtype[@racket[char-string] @elem{byte string or @racket[#f]} @cpp{char*}
           @toC{string @|goesto| contained byte-array pointer, @racket[#f] @|goesto| @cpp{NULL}}
           @toRacket{@cpp{NULL} @|goesto| @racket[#f], anything else @|goesto| new byte string created by copying the string}]

  @argtype[@racket[nonnull-char-string] "byte string" @cpp{char*}
           @toC{byte string's contained byte-array pointer}
           @toRacket{new byte string created by copying the string}]

  @argtype[@racket[racket-object] "any value" @cpp{Racket_Object*}
           @toC{no conversion}
           @toRacket{no conversion}]

  @argtype[@racket[(pointer _bstr)] @elem{an opaque c-pointer value, identified as type @racket[bstr], or @racket[#f]} @cpp{@racket[_bstr]*}
           @toC{@racket[#f] @|goesto| @cpp{NULL}, c-pointer @|goesto| contained pointer cast to @cpp{@racket[_bstr]*}}
           @toRacket{@cpp{NULL} @|goesto| @racket[#f], anything else @|goesto| new c-pointer containing the pointer 
                     and identified as type @racket[_bstr]}]

]

The @racket[return-type] must be @racketidfont{void} or one of the
@racket[arg-type] keywords.}

@defform[(c-declare code-string)]{

Declares arbitrary C code to appear after @filepath{escheme.h} or
@filepath{scheme.h} is included, but before any other code in the
compilation environment of the declaration. A @racket[c-declare] form
can appear only at the top-level or within a module's top-level
sequence.

The @racket[code] code will appear on a new line in the file for C
compilation. Multiple @racket[c-include] declarations are concatenated
(with newlines) in order to produces a sequence of declarations.}

@defform[(c-include path-spec)]{

Expands to a use of @racket[c-declare] with the content of
@racket[path-spec]. The @racket[path-spec] has the same form as for
@racketmodname[mzlib/include]'s @racket[include].}

@(bibliography
  (bib-entry
   #:key "Feeley98"
   #:author "Marc Feeley"
   #:title "Gambit-C, version 3.0"
   #:date "1998"))
