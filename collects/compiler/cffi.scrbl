#lang scribble/doc
@(require scribble/manual
          scribble/struct
          (for-label scheme/base
                     compiler/cffi
                     mzlib/include))

@(define mzc (exec "mzc"))
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
                @elem{Scheme range: @|desc|}
                @elem{C type: @|c|}
                cases)))

@(define (toC . content)
   (apply elem "Scheme to C conversion: " content))
@(define (toScheme . content)
   (apply elem "C to Scheme conversion: " content))
@(define tosObvious
   (elem "conversions: (obvious and precise)"))
@(define goesto 'rarr)

   

@title{@scheme[c-lambda]: C FFI via @exec{mzc}}

@defmodule[compiler/cffi]{

The @schememodname[compiler/cffi] module relies on a C compiler to
statically construct an interface to C code through directives
embedded in a Scheme program. The library implements a subset of
@as-index{Gambit-C}'s foreign-function interface @cite["Feeley98"].}

The @schememodname[scheme/foreign] library is a better interface for
most tasks; see @other-manual['(lib
"scribblings/foreign/foreign.scrbl")] for more information on
@schememodname[scheme/foreign]. See also @|inside|, which describes
PLT Scheme's C-level API for extending the run-time system.

The @schememodname[compiler/cffi] library defines three forms:
@scheme[c-lambda], @scheme[c-declare], and @scheme[c-include]. When
interpreted directly or compiled to byte code, @scheme[c-lambda]
produces a function that always raises @scheme[exn:fail], and
@scheme[c-declare] and @scheme[c-include] raise
@scheme[exn:fail]. When compiled by @exec{mzc --extension}, the forms
provide access to C. Thus, @schememodname[compiler/cffi] is normally
required by a module to be compiled via @|mzc|. In addition, the
@|mzc| compiler implicitly imports @schememodname[compiler/cffi] into
the top-level environment for non-@scheme[module] compilation.

The @scheme[c-lambda] form creates a Scheme procedure whose body is
implemented in C. Instead of declaring argument names, a
@scheme[c-lambda] form declares argument types, as well as a return
type. The implementation can be simply the name of a C function, as in
the following definition of @scheme[fmod]:

@schemeblock[
(define fmod (c-lambda (double double) double "fmod"))
]

Alternatively, the implementation can be C code to serve as the body
of a function, where the arguments are bound to @cpp{___arg1} (three
underscores), etc., and the result is installed into @cpp{___result}
(three underscores):

@schemeblock[
(define machine-string->float
  (c-lambda (char-string) float
     "___result = *(float *)___arg1;"))
]

The @scheme[c-lambda] form provides only limited conversions between C
and Scheme data. For example, the following function does not reliably
produce a string of four characters:

@schemeblock[
(define broken-machine-float->string
  (c-lambda (float) char-string
     "char b[5]; *(float *)b = ___arg1; b[4] = 0; ___result = b;"))
]

because the representation of a @cpp{float} can contain null bytes,
which terminate the string. However, the full MzScheme API, which is
described in @|inside|, can be used in a function body:

@schemeblock[
(define machine-float->string
  (c-lambda (float) scheme-object
     "char b[4];"
     "*(float *)b = ___arg1;"
     "___result = scheme_make_sized_byte_string(b, 4, 1);"))
]

The @scheme[c-declare] form declares arbitrary C code to appear after
@filepath{escheme.h} or @filepath{scheme.h} is included, but before
any other code in the compilation environment of the declaration. It
is often used to declare C header file inclusions. For example, a
proper definition of @scheme[fmod] needs the @filepath{math.h} header
file:

@schemeblock[
(c-declare "#include <math.h>")
(define fmod (c-lambda (double double) double "fmod"))
]

The @scheme[c-declare] form can also be used to define helper C
functions to be called through @scheme[c-lambda].

The @scheme[c-include] form expands to a @scheme[c-declare] form using
the content of a specified file. Use @scheme[(c-include _file)] instead
of @scheme[(c-declare "#include file")] when it's easier to
have MzScheme resolve the file path than to have the C compiler
resolve it.

The @filepath{plt/collects/mzscheme/examples} directory in the PLT
distribution contains additional examples.

When compiling for MzScheme3m (see @|inside|), C code inserted by
@scheme[c-lambda], @scheme[c-declare], and @scheme[c-include] will be
transformed in the same was as @|mzc|'s @DFlag{xform} mode (which may
or may not be enough to make the code work correctly in MzScheme3m;
see @|inside| for more information).


@defform[(c-lambda (argument-type ...) return-type impl-string ...+)]{

Creates a Scheme procedure whose body is implemented in C. The
procedure takes as many arguments as the supplied
@scheme[argument-type]s, and it returns one value. If
@scheme[return-type] is @schemeidfont{void}, the procedure's result is
always void.  The @scheme[impl-string] is either the name of a C
function (or macro) or the body of a C function.

If a single @scheme[impl-string] is provided, and if it is a
string containing only alphanumeric characters and @litchar{_},
then the created Scheme procedure passes all of its arguments to
the named C function (or macro) and returns the function's
result. Each argument to the Scheme procedure is converted
according to the corresponding @scheme[argument-type] (as
described below) to produce an argument to the C function. Unless
@scheme[return-type] is @schemeidfont{void}, the C function's result is
converted according to @scheme[return-type] for the Scheme
procedure's result.

If more than @scheme[impl-string] is provided, or if it contains more
than alphanumeric characters and @litchar{_}, then the concatenated
@scheme[impl-string]s must contain C code to implement the function
body. The converted arguments for the function will be in variables
@cpp{___arg1}, @cpp{___arg2}, ... (with three underscores in each
name) in the context where the @scheme[impl-string]s are placed for
compilation. Unless @scheme[return-type] is @schemeidfont{void}, the
@scheme[impl-string]s code should assign a result to the variable
@cpp{___result} (three underscores), which will be declared but not
initialized. The @scheme[impl-string]s code should not return
explicitly; control should always reach the end of the body. If the
@scheme[impl-string]s code defines the pre-processor macro
@cpp{___AT_END} (with three leading underscores), then the macro's
value should be C code to execute after the value @cpp{___result} is
converted to a Scheme result, but before the result is returned, all
in the same block; defining @cpp{___AT_END} is primarily useful for
deallocating a string in @cpp{___result} that has been copied by
conversion.  The @scheme[impl-string]s code will start on a new line at
the beginning of a block in its compilation context, and
@cpp{___AT_END} will be undefined after the code.

In addition to @cpp{___arg1}, etc., the variable @cpp{argc} is bound
in @scheme[impl-string]s to the number of arguments supplied to
the function, and @cpp{argv} is bound to a @cpp{Scheme_Object*} array
of length @cpp{argc} containing the function arguments as Scheme
values. The @cpp{argv} and @cpp{argc} variables are mainly useful for
error reporting (e.g., with @cpp{scheme_wrong_type}).

Each @scheme[argument-type] must be one of the following, which are
recognized symbolically:

@itemize{

  @argtype[@scheme[bool] "any value" @cpp{int}
           @toC{@scheme[#f] @|goesto| 0, anything else @|goesto| 1}
           @toScheme{0 @|goesto| @scheme[#f], anything else @|goesto| @scheme[#t]}]

  @argtype[@scheme[char] "character" @cpp{char}
           @toC{character's Latin-1 value cast to signed byte}
           @toScheme{Latin-1 value from unsigned cast mapped to character}]

  @argtype[@scheme[unsigned-char] "character" @cpp{unsigned char}
           @toC{character's Latin-1 value}
           @toScheme{Latin-1 value mapped to character}]

  @argtype[@scheme[signed-char] "character" @cpp{signed char}
           @toC{character's Latin-1 value cast to signed byte}
           @toScheme{Latin-1 value from unsigned cast mapped to character}]

  @argtype[@scheme[int] @elem{exact integer that fits into an @cpp{int}} @cpp{int}
           @tosObvious]

  @argtype[@scheme[unsigned-int] @elem{exact integer that fits into an @cpp{unsigned int}} @cpp{unsigned int}
           @tosObvious]

  @argtype[@scheme[long] @elem{exact integer that fits into a @cpp{long}} @cpp{long}
           @tosObvious]

  @argtype[@scheme[unsigned-long] @elem{exact integer that fits into an @cpp{unsigned long}} @cpp{unsigned long}
           @tosObvious]

  @argtype[@scheme[short] @elem{exact integer that fits into a @cpp{short}} @cpp{short}
           @tosObvious]

  @argtype[@scheme[unsigned-short] @elem{exact integer that fits into an @cpp{unsigned short}} @cpp{unsigned short}
           @tosObvious]

  @argtype[@scheme[float] "real number" @cpp{float}
           @toC{number converted to inexact and cast to @cpp{float}}
           @toScheme{cast to @cpp{double} and encapsulated as an inexact number}]

  @argtype[@scheme[double] "real number" @cpp{double}
           @toC{number converted to inexact}
           @toScheme{encapsulated as an inexact number}]

  @argtype[@scheme[char-string] @elem{byte string or @scheme[#f]} @cpp{char*}
           @toC{string @|goesto| contained byte-array pointer, @scheme[#f] @|goesto| @cpp{NULL}}
           @toScheme{@cpp{NULL} @|goesto| @scheme[#f], anything else @|goesto| new byte string created by copying the string}]

  @argtype[@scheme[nonnull-char-string] "byte string" @cpp{char*}
           @toC{byte string's contained byte-array pointer}
           @toScheme{new byte string created by copying the string}]

  @argtype[@scheme[scheme-object] "any value" @cpp{Scheme_Object*}
           @toC{no conversion}
           @toScheme{no conversion}]

  @argtype[@scheme[(pointer _bstr)] @elem{an opaque c-pointer value, identified as type @scheme[bstr], or @scheme[#f]} @cpp{@scheme[_bstr]*}
           @toC{@scheme[#f] @|goesto| @cpp{NULL}, c-pointer @|goesto| contained pointer cast to @cpp{@scheme[_bstr]*}}
           @toScheme{@cpp{NULL} @|goesto| @scheme[#f], anything else @|goesto| new c-pointer containing the pointer 
                     and identified as type @scheme[_bstr]}]

}

The @scheme[return-type] must be @schemeidfont{void} or one of the
@scheme[arg-type] keywords.}

@defform[(c-declare code-string)]{

Declares arbitrary C code to appear after @filepath{escheme.h} or
@filepath{scheme.h} is included, but before any other code in the
compilation environment of the declaration. A @scheme[c-declare] form
can appear only at the top-level or within a module's top-level
sequence.

The @scheme[code] code will appear on a new line in the file for C
compilation. Multiple @scheme[c-include] declarations are concatenated
(with newlines) in order to produces a sequence of declarations.}

@defform[(c-include path-spec)]{

Expands to a use of @scheme[c-declare] with the content of
@scheme[path-spec]. The @scheme[path-spec] has the same form as for
@schememodname[mzlib/include]'s @scheme[include].}

@(bibliography
  (bib-entry
   #:key "Feeley98"
   #:author "Marc Feeley"
   #:title "Gambit-C, version 3.0"
   #:date "1998"))
