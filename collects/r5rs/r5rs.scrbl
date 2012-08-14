#lang scribble/doc
@(require scribble/manual
          (for-label (only-meta-in 0 r5rs)
                     (only-in r5rs syntax-rules ...)
                     (only-in mzscheme #%plain-module-begin)
                     (only-in scheme/mpair mmap)
                     (only-in scheme/contract one-of/c)
                     (only-in scheme/base
                              require find-system-path namespace? mcons mcdr
                              namespace-require namespace-require/copy
                              read-case-sensitive
                              read-accept-infix-dot
                              read-curly-brace-as-paren
                              read-square-bracket-as-paren
                              print-vector-length
                              print-mpair-curly-braces)))


@(define r5rs @elem{R@superscript{5}RS})
@(define drs-doc '(lib "scribblings/drracket/drracket.scrbl"))

@title{R5RS: Legacy Scheme}

The @link["../r5rs-std/index.html"]{The Revised@superscript{5} Report
on the Algorithmic Language Scheme} defines a dialect of Scheme. We
use @defterm{@|r5rs|} to refer to both the standard and the language
defined by the standard.

@margin-note{See @seclink[#:doc '(lib "scribblings/guide/guide.scrbl")
             "dialects"] for general information about different
             dialects of Scheme within Racket.}

The default dialect of Lisp provided by @exec{racket} and other
Racket tools differs from @|r5rs| in many ways, but Racket
includes tools and libraries for running @|r5rs| programs.

@table-of-contents[]

@redirects[]

@; ----------------------------------------

@section[#:tag "running"]{Running @|r5rs| Programs}

Racket provides several layers of support for programs written
according to @|r5rs|:

@itemize[

 @item{DrRacket provides an @onscreen{R5RS} language, which can be
       selected via the @menuitem["Language" "Choose Language..."]
       menu item. See @secref[#:doc drs-doc "choose-language"] in
       @other-manual[drs-doc] for more information.}

 @item{The @exec{plt-r5rs} executable runs an @|r5rs| program or
       provides a read-eval-print loop for evaluating @|r5rs|
       expressions and definitions. See @secref["plt-r5rs"] (later in
       this manual) for more information.}

 @item{The @racketmodname[r5rs] library implemented @|r5rs| procedures
       and syntactic forms. It can also be used with @hash-lang[] to
       create a module whose body is implemented in an @|r5rs|-like
       language.  See @secref["r5rs-mod"] (later in this manual) for
       more information.}

 @item{The @racketmodname[r5rs/init] library extends
       @racketmodname[r5rs] to set parameters (such as
       case-insensitive symbol reading) for @|r5rs| loading or an
       @|r5rs| read-eval-print loop. See @secref["r5rs/init-mod"]
       (later in this manual) for more information.}

]

@; ----------------------------------------

@section[#:tag "plt-r5rs"]{@exec{plt-r5rs}}

The @exec{plt-r5rs} executable runs an @|r5rs| program from a file
that is supplied on the command line. If no program file is provided
as a command-line argument, then a read-eval-print loop is started.

Before starting a read-eval-print loop, an initialization file is
loaded, if it exists. The file is the same as the file reported by
@racket[(find-system-path 'init-file)], but with the characters
@litchar{racket} in the filename replaced by @litchar{pltr5rs}. For
example, on Unix, the file is @filepath{~/.pltr5rsrc}.

By default, @exec{plt-r5rs} departs from @|r5rs| conformance in one
crucial way: the names of pre-defined functions cannot be redefined at
the top level. This restriction enables better run-time
performance. Use the @as-index{@DFlag{no-prim}} command-line
flag---before a file to load, if any---to obtain the standard behavior
for primitive bindings (at the cost of performance).

@; ----------------------------------------

@section[#:tag "r5rs-mod"]{@|r5rs| Module Language}

@defmodulelang[r5rs]

As a library, @racketmodname[r5rs] provides the syntactic forms and
procedures defined by @|r5rs|. When used as a language via
@hash-lang[], the program is read with the following
parameterizations:

@racketblock[
  (read-case-sensitive #f)
  (read-accept-infix-dot #f)
  (read-curly-brace-as-paren #f)
  (read-square-bracket-as-paren #f)
]

The @racketmodname[r5rs] bindings can be imported into a top-level
environment, and then evaluation in that top-level environment
corresponds to @|r5rs|. Use @racket[(namespace-require/copy 'r5rs)]
with an empty namespace to maximize conformance with @|r5rs|; Using
@racket[(namespace-require 'r5rs)], in contrast, creates primitive
bindings as imports, which is the same as using
@seclink["plt-r5rs"]{@exec{plt-r5rs}} without the @DFlag{no-prim} flag.
More simply, use @racket[(scheme-report-environment 5)].  See also
@racketmodname[r5rs/init], which sets reader and printer parameters to
increase conformance.

Using @racketmodname[r5rs] via @hash-lang[] creates a module whose
body is implemented with an @|r5rs|-like language. The main difference
from @|r5rs| is that, as a module language, @racketmodname[r5rs] does
not allow redefinition of top-level bindings, and expressions
evaluated through @racket[load] and @racket[eval] cannot automatically
access bindings defined within the module.

@; --------------------

@subsection{Non-@|r5rs| Bindings from @racketmodname[r5rs]}

In addition to the bindings defined by @|r5rs|, the
@racketmodname[r5rs] library provides the following bindings from
@racketmodname[racket/base] (which are not legal identifiers in @|r5rs|
syntax, so there is no danger of collisions in @|r5rs| programs):

@racketblock[
#%app #%datum #%top #%top-interaction #%require #%provide
]

It also provides @racketmodname[racket]'s
@racket[#%plain-module-begin] as @racket[#%module-begin]. Note that
@racket[#%require] can be used to import Racket libraries into an
otherwise @|r5rs| program, and @racket[#%provide] can be used to
export from a module that is implemented in an @|r5rs|-like language.

@; --------------------

@subsection{Notes on @|r5rs| Functions}

The @racket[cons] of @racketmodname[r5rs] corresponds to
@racketmodname[racket/base]'s @racket[mcons]. Similarly, @racket[cdr]
is @racket[mcdr], and @racket[map] is @racketmodname[compatibility/mlist]'s
@racket[mmap], and so on.

An @|r5rs| @defterm{environment} is implemented as a
@racket[racket/base] @defterm{namespace}. Also, relative to
@racket[racket/base], the @racket[expr] passed to @racket[eval] is
constructed using mutable pairs.

The @racket[scheme-report-environment] function returns a namespace
containing the bindings of @racketmodname[r5rs].  Procedure values are
installed into the namespace using @racket[namespace-require/copy], so
that they can be redefined.

The @racket[null-environment] function returns a namespace
containing the syntactic forms of @racketmodname[r5rs], not including
@racket[#%module-begin] (which is not useful outside of a module).

@; ----------------------------------------

@section[#:tag "r5rs/init-mod"]{@|r5rs| Initialization Library}

@defmodule[r5rs/init]

The @racketmodname[r5rs/init] module re-exports @racketmodname[r5rs],
and also sets parameters as follows:

@racketblock[
  (read-case-sensitive #f)
  (read-accept-infix-dot #f)
  (read-curly-brace-as-paren #f)
  (read-square-bracket-as-paren #f)
  (print-mpair-curly-braces #f)
]

The side-effect of setting these parameters is useful when the module
is @racket[require]d before loading an @|r5rs| program, so that the
reader and printer behave more as specified in @|r5rs|. In particular,
the @seclink["plt-r5rs"]{@exec{plt-r5rs} executable} initializes by
importing @racketmodname[r5rs/init].

@; ----------------------------------------

@(define (redirects)
  (make-binding-redirect-elements
   'r5rs
   (map (lambda (b)
          (list (string->symbol (car b))
                (cadr b)
                (build-path 'up "r5rs-std" (caddr b))
                (cadddr b)))
        bindings)))

@; This list was extracted from the old "keywords" and "hdindex" files:
@(define bindings
  '(("lambda" #t "r5rs-Z-H-7.html" "%_idx_96")
    ("set!" #t "r5rs-Z-H-7.html" "%_idx_104")
    ("quote" #t "r5rs-Z-H-7.html" "%_idx_86")
    ("quasiquote" #t "r5rs-Z-H-7.html" "%_idx_154")
    ("or" #t "r5rs-Z-H-7.html" "%_idx_122")
    ("and" #t "r5rs-Z-H-7.html" "%_idx_120")
    ("define" #t "r5rs-Z-H-8.html" "%_idx_194")
    ("define-syntax" #t "r5rs-Z-H-8.html" "%_idx_202")
    ("let" #t "r5rs-Z-H-7.html" "%_idx_126")
    ("let*" #t "r5rs-Z-H-7.html" "%_idx_130")
    ("let-syntax" #t "r5rs-Z-H-7.html" "%_idx_184")
    ("letrec" #t "r5rs-Z-H-7.html" "%_idx_134")
    ("letrec-syntax" #t "r5rs-Z-H-7.html" "%_idx_186")
    ("cond" #t "r5rs-Z-H-7.html" "%_idx_108")
    ("delay" #t "r5rs-Z-H-7.html" "%_idx_146")
    ("do" #t "r5rs-Z-H-7.html" "%_idx_140")
    ("if" #t "r5rs-Z-H-7.html" "%_idx_98")
    ("case" #t "r5rs-Z-H-7.html" "%_idx_116")
    ("begin" #t "r5rs-Z-H-7.html" "%_idx_138")

    ("zero?" #f "r5rs-Z-H-9.html" "%_idx_268")
    ("write-char" #f "r5rs-Z-H-9.html" "%_idx_666")
    ;; ("write-char" #f "r5rs-Z-H-9.html" "%_idx_664")
    ("write" #f "r5rs-Z-H-9.html" "%_idx_654")
    ;; ("write" #f "r5rs-Z-H-9.html" "%_idx_652")
    ("with-output-to-file" #f "r5rs-Z-H-9.html" "%_idx_624")
    ("with-input-from-file" #f "r5rs-Z-H-9.html" "%_idx_622")
    ("vector?" #f "r5rs-Z-H-9.html" "%_idx_556")
    ("vector-set!" #f "r5rs-Z-H-9.html" "%_idx_568")
    ("vector-ref" #f "r5rs-Z-H-9.html" "%_idx_566")
    ("vector-length" #f "r5rs-Z-H-9.html" "%_idx_564")
    ("vector-fill!" #f "r5rs-Z-H-9.html" "%_idx_574")
    ("vector->list" #f "r5rs-Z-H-9.html" "%_idx_570")
    ("vector" #f "r5rs-Z-H-9.html" "%_idx_562")
    ("values" #f "r5rs-Z-H-9.html" "%_idx_594")
    ("truncate" #f "r5rs-Z-H-9.html" "%_idx_318")
    ("transcript-on" #f "r5rs-Z-H-9.html" "%_idx_670")
    ("transcript-off" #f "r5rs-Z-H-9.html" "%_idx_672")
    ("tan" #f "r5rs-Z-H-9.html" "%_idx_334")
    ("symbol?" #f "r5rs-Z-H-9.html" "%_idx_460")
    ("symbol->string" #f "r5rs-Z-H-9.html" "%_idx_462")
    ("substring" #f "r5rs-Z-H-9.html" "%_idx_542")
    ("string?" #f "r5rs-Z-H-9.html" "%_idx_508")
    ("string>?" #f "r5rs-Z-H-9.html" "%_idx_528")
    ("string>=?" #f "r5rs-Z-H-9.html" "%_idx_532")
    ("string=?" #f "r5rs-Z-H-9.html" "%_idx_522")
    ("string<?" #f "r5rs-Z-H-9.html" "%_idx_526")
    ("string<=?" #f "r5rs-Z-H-9.html" "%_idx_530")
    ("string-set!" #f "r5rs-Z-H-9.html" "%_idx_520")
    ("string-ref" #f "r5rs-Z-H-9.html" "%_idx_518")
    ("string-length" #f "r5rs-Z-H-9.html" "%_idx_516")
    ("string-fill!" #f "r5rs-Z-H-9.html" "%_idx_552")
    ("string-copy" #f "r5rs-Z-H-9.html" "%_idx_550")
    ("string-ci>?" #f "r5rs-Z-H-9.html" "%_idx_536")
    ("string-ci>=?" #f "r5rs-Z-H-9.html" "%_idx_540")
    ("string-ci=?" #f "r5rs-Z-H-9.html" "%_idx_524")
    ("string-ci<?" #f "r5rs-Z-H-9.html" "%_idx_534")
    ("string-ci<=?" #f "r5rs-Z-H-9.html" "%_idx_538")
    ("string-append" #f "r5rs-Z-H-9.html" "%_idx_544")
    ("string->symbol" #f "r5rs-Z-H-9.html" "%_idx_464")
    ("string->number" #f "r5rs-Z-H-9.html" "%_idx_370")
    ;; ("string->number" #f "r5rs-Z-H-9.html" "%_idx_368")
    ("string->list" #f "r5rs-Z-H-9.html" "%_idx_546")
    ("string" #f "r5rs-Z-H-9.html" "%_idx_514")
    ("sqrt" #f "r5rs-Z-H-9.html" "%_idx_344")
    ("sin" #f "r5rs-Z-H-9.html" "%_idx_330")
    ("set-cdr!" #f "r5rs-Z-H-9.html" "%_idx_418")
    ("set-car!" #f "r5rs-Z-H-9.html" "%_idx_416")
    ("scheme-report-environment" #f "r5rs-Z-H-9.html" "%_idx_602")
    ("round" #f "r5rs-Z-H-9.html" "%_idx_320")
    ("reverse" #f "r5rs-Z-H-9.html" "%_idx_440")
    ("remainder" #f "r5rs-Z-H-9.html" "%_idx_302")
    ("real?" #f "r5rs-Z-H-9.html" "%_idx_248")
    ("real-part" #f "r5rs-Z-H-9.html" "%_idx_352")
    ("read-char" #f "r5rs-Z-H-9.html" "%_idx_640")
    ;; ("read-char" #f "r5rs-Z-H-9.html" "%_idx_638")
    ("read" #f "r5rs-Z-H-9.html" "%_idx_636")
    ;; ("read" #f "r5rs-Z-H-9.html" "%_idx_634")
    ("rationalize" #f "r5rs-Z-H-9.html" "%_idx_322")
    ("rational?" #f "r5rs-Z-H-9.html" "%_idx_250")
    ("quotient" #f "r5rs-Z-H-9.html" "%_idx_300")
    ("procedure?" #f "r5rs-Z-H-9.html" "%_idx_576")
    ("positive?" #f "r5rs-Z-H-9.html" "%_idx_270")
    ("peek-char" #f "r5rs-Z-H-9.html" "%_idx_644")
    ;; ("peek-char" #f "r5rs-Z-H-9.html" "%_idx_642")
    ("pair?" #f "r5rs-Z-H-9.html" "%_idx_406")
    ("output-port?" #f "r5rs-Z-H-9.html" "%_idx_616")
    ("open-output-file" #f "r5rs-Z-H-9.html" "%_idx_628")
    ("open-input-file" #f "r5rs-Z-H-9.html" "%_idx_626")
    ("odd?" #f "r5rs-Z-H-9.html" "%_idx_274")
    ("numerator" #f "r5rs-Z-H-9.html" "%_idx_310")
    ("number?" #f "r5rs-Z-H-9.html" "%_idx_244")
    ("number->string" #f "r5rs-Z-H-9.html" "%_idx_366")
    ;; ("number->string" #f "r5rs-Z-H-9.html" "%_idx_364")
    ("null?" #f "r5rs-Z-H-9.html" "%_idx_428")
    ("null-environment" #f "r5rs-Z-H-9.html" "%_idx_604")
    ("not" #f "r5rs-Z-H-9.html" "%_idx_386")
    ("newline" #f "r5rs-Z-H-9.html" "%_idx_662")
    ;; ("newline" #f "r5rs-Z-H-9.html" "%_idx_660")
    ("negative?" #f "r5rs-Z-H-9.html" "%_idx_272")
    ("modulo" #f "r5rs-Z-H-9.html" "%_idx_304")
    ("min" #f "r5rs-Z-H-9.html" "%_idx_280")
    ("memv" #f "r5rs-Z-H-9.html" "%_idx_448")
    ("memq" #f "r5rs-Z-H-9.html" "%_idx_446")
    ("member" #f "r5rs-Z-H-9.html" "%_idx_450")
    ("max" #f "r5rs-Z-H-9.html" "%_idx_278")
    ("map" #f "r5rs-Z-H-9.html" "%_idx_580")
    ("make-vector" #f "r5rs-Z-H-9.html" "%_idx_560")
    ;; ("make-vector" #f "r5rs-Z-H-9.html" "%_idx_558")
    ("make-string" #f "r5rs-Z-H-9.html" "%_idx_512")
    ;; ("make-string" #f "r5rs-Z-H-9.html" "%_idx_510")
    ("make-rectangular" #f "r5rs-Z-H-9.html" "%_idx_348")
    ("make-polar" #f "r5rs-Z-H-9.html" "%_idx_350")
    ("magnitude" #f "r5rs-Z-H-9.html" "%_idx_356")
    ("log" #f "r5rs-Z-H-9.html" "%_idx_328")
    ("load" #f "r5rs-Z-H-9.html" "%_idx_668")
    ("list?" #f "r5rs-Z-H-9.html" "%_idx_432")
    ("list-tail" #f "r5rs-Z-H-9.html" "%_idx_442")
    ("list-ref" #f "r5rs-Z-H-9.html" "%_idx_444")
    ("list->vector" #f "r5rs-Z-H-9.html" "%_idx_572")
    ("list->string" #f "r5rs-Z-H-9.html" "%_idx_548")
    ("list" #f "r5rs-Z-H-9.html" "%_idx_434")
    ("length" #f "r5rs-Z-H-9.html" "%_idx_436")
    ("lcm" #f "r5rs-Z-H-9.html" "%_idx_308")
    ("interaction-environment" #f "r5rs-Z-H-9.html" "%_idx_606")
    ("integer?" #f "r5rs-Z-H-9.html" "%_idx_252")
    ("integer->char" #f "r5rs-Z-H-9.html" "%_idx_500")
    ("input-port?" #f "r5rs-Z-H-9.html" "%_idx_614")
    ("inexact?" #f "r5rs-Z-H-9.html" "%_idx_256")
    ("inexact->exact" #f "r5rs-Z-H-9.html" "%_idx_362")
    ("imag-part" #f "r5rs-Z-H-9.html" "%_idx_354")
    ("gcd" #f "r5rs-Z-H-9.html" "%_idx_306")
    ("force" #f "r5rs-Z-H-9.html" "%_idx_584")
    ("for-each" #f "r5rs-Z-H-9.html" "%_idx_582")
    ("floor" #f "r5rs-Z-H-9.html" "%_idx_314")
    ("expt" #f "r5rs-Z-H-9.html" "%_idx_346")
    ("exp" #f "r5rs-Z-H-9.html" "%_idx_326")
    ("exact?" #f "r5rs-Z-H-9.html" "%_idx_254")
    ("exact->inexact" #f "r5rs-Z-H-9.html" "%_idx_360")
    ("even?" #f "r5rs-Z-H-9.html" "%_idx_276")
    ("eval" #f "r5rs-Z-H-9.html" "%_idx_600")
    ("eqv?" #f "r5rs-Z-H-9.html" "%_idx_214")
    ("equal?" #f "r5rs-Z-H-9.html" "%_idx_222")
    ("eq?" #f "r5rs-Z-H-9.html" "%_idx_220")
    ("eof-object?" #f "r5rs-Z-H-9.html" "%_idx_646")
    ("dynamic-wind" #f "r5rs-Z-H-9.html" "%_idx_598")
    ("display" #f "r5rs-Z-H-9.html" "%_idx_658")
    ;; ("display" #f "r5rs-Z-H-9.html" "%_idx_656")
    ("denominator" #f "r5rs-Z-H-9.html" "%_idx_312")
    ("current-output-port" #f "r5rs-Z-H-9.html" "%_idx_620")
    ("current-input-port" #f "r5rs-Z-H-9.html" "%_idx_618")
    ("cos" #f "r5rs-Z-H-9.html" "%_idx_332")
    ("cons" #f "r5rs-Z-H-9.html" "%_idx_408")
    ("complex?" #f "r5rs-Z-H-9.html" "%_idx_246")
    ("close-output-port" #f "r5rs-Z-H-9.html" "%_idx_632")
    ("close-input-port" #f "r5rs-Z-H-9.html" "%_idx_630")
    ("char?" #f "r5rs-Z-H-9.html" "%_idx_466")
    ("char>?" #f "r5rs-Z-H-9.html" "%_idx_472")
    ("char>=?" #f "r5rs-Z-H-9.html" "%_idx_476")
    ("char=?" #f "r5rs-Z-H-9.html" "%_idx_468")
    ("char<?" #f "r5rs-Z-H-9.html" "%_idx_470")
    ("char<=?" #f "r5rs-Z-H-9.html" "%_idx_474")
    ("char-whitespace?" #f "r5rs-Z-H-9.html" "%_idx_492")
    ("char-upper-case?" #f "r5rs-Z-H-9.html" "%_idx_494")
    ("char-upcase" #f "r5rs-Z-H-9.html" "%_idx_502")
    ("char-ready?" #f "r5rs-Z-H-9.html" "%_idx_650")
    ;; ("char-ready?" #f "r5rs-Z-H-9.html" "%_idx_648")
    ("char-numeric?" #f "r5rs-Z-H-9.html" "%_idx_490")
    ("char-lower-case?" #f "r5rs-Z-H-9.html" "%_idx_496")
    ("char-downcase" #f "r5rs-Z-H-9.html" "%_idx_504")
    ("char-ci>?" #f "r5rs-Z-H-9.html" "%_idx_482")
    ("char-ci>=?" #f "r5rs-Z-H-9.html" "%_idx_486")
    ("char-ci=?" #f "r5rs-Z-H-9.html" "%_idx_478")
    ("char-ci<?" #f "r5rs-Z-H-9.html" "%_idx_480")
    ("char-ci<=?" #f "r5rs-Z-H-9.html" "%_idx_484")
    ("char-alphabetic?" #f "r5rs-Z-H-9.html" "%_idx_488")
    ("char->integer" #f "r5rs-Z-H-9.html" "%_idx_498")
    ("ceiling" #f "r5rs-Z-H-9.html" "%_idx_316")
    ("cdr" #f "r5rs-Z-H-9.html" "%_idx_414")
    ("cddddr" #f "r5rs-Z-H-9.html" "%_idx_426")
    ("cdddar" #f "r5rs-Z-H-9.html" "%_idx_424")
    ("car" #f "r5rs-Z-H-9.html" "%_idx_410")
    ("call-with-values" #f "r5rs-Z-H-9.html" "%_idx_596")
    ("call-with-output-file" #f "r5rs-Z-H-9.html" "%_idx_612")
    ("call-with-input-file" #f "r5rs-Z-H-9.html" "%_idx_610")
    ("call-with-current-continuation" #f "r5rs-Z-H-9.html" "%_idx_588")
    ("cadr" #f "r5rs-Z-H-9.html" "%_idx_422")
    ("caar" #f "r5rs-Z-H-9.html" "%_idx_420")
    ("boolean?" #f "r5rs-Z-H-9.html" "%_idx_388")
    ("atan" #f "r5rs-Z-H-9.html" "%_idx_342")
    ;; ("atan" #f "r5rs-Z-H-9.html" "%_idx_340")
    ("assv" #f "r5rs-Z-H-9.html" "%_idx_454")
    ("assq" #f "r5rs-Z-H-9.html" "%_idx_452")
    ("assoc" #f "r5rs-Z-H-9.html" "%_idx_456")
    ("asin" #f "r5rs-Z-H-9.html" "%_idx_336")
    ("apply" #f "r5rs-Z-H-9.html" "%_idx_578")
    ("append" #f "r5rs-Z-H-9.html" "%_idx_438")
    ("angle" #f "r5rs-Z-H-9.html" "%_idx_358")
    ("acos" #f "r5rs-Z-H-9.html" "%_idx_338")
    ("abs" #f "r5rs-Z-H-9.html" "%_idx_298")
    (">=" #f "r5rs-Z-H-9.html" "%_idx_266")
    (">" #f "r5rs-Z-H-9.html" "%_idx_262")
    ("=" #f "r5rs-Z-H-9.html" "%_idx_258")
    ("<=" #f "r5rs-Z-H-9.html" "%_idx_264")
    ("<" #f "r5rs-Z-H-9.html" "%_idx_260")
    ("/" #f "r5rs-Z-H-9.html" "%_idx_296")
    ;;("/" #f "r5rs-Z-H-9.html" "%_idx_294")
    ;;("/" #f "r5rs-Z-H-9.html" "%_idx_292")
    ("-" #f "r5rs-Z-H-9.html" "%_idx_290")
    ;;("-" #f "r5rs-Z-H-9.html" "%_idx_288")
    ;;("-" #f "r5rs-Z-H-9.html" "%_idx_286")
    ("+" #f "r5rs-Z-H-9.html" "%_idx_282")
    ("*" #f "r5rs-Z-H-9.html" "%_idx_284")))
