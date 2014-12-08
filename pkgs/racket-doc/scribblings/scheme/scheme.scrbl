#lang scribble/manual
@(require (for-label (only-in scheme/foreign unsafe! provide* define-unsafer)
                     (only-in scheme/base make-base-namespace make-base-empty-namespace #%module-begin)
                     (only-in scheme/pretty pretty-print)
                     (only-in racket/pretty pretty-write)
                     (only-in scheme/class printable<%>)
                     (only-in racket/class writable<%>)
                     (only-in racket/base struct hash hasheq hasheqv in-directory local-require)
                     (only-in racket/unit struct/ctc)
                     (only-in scheme/gui/base make-gui-namespace make-gui-empty-namespace)
                     (only-in scheme/gui/dynamic gui-dynamic-require)
                     (only-in mzlib/unit struct~s struct~s/ctc)
                     scheme/gui/base
                     scheme/sandbox))

@(define-syntax-rule (def-extras unit-struct
                                 unit-struct/ctc
                                 make-base-namespace-id
                                 make-base-empty-namespace-id
                                 sandbox-namespace-specs-id
                                 make-evaluator-id
                                 make-module-evaluator-id
                                 module-begin-id
                                 pretty-print-id
                                 printable<%>-id
                                 gui-dynamic-require-id)
  (begin
    (require (for-label (only-in scheme struct struct/ctc)
                        (only-in racket/base make-base-namespace
                                             make-base-empty-namespace
                                             #%module-begin)
                        (only-in racket/pretty pretty-print)
                        (only-in racket/gui/dynamic gui-dynamic-require)
                        racket/sandbox))
    (define unit-struct (racket struct))
    (define unit-struct/ctc (racket struct/ctc))
    (define make-base-namespace-id (racket make-base-namespace))
    (define make-base-empty-namespace-id (racket make-base-empty-namespace))
    (define sandbox-namespace-specs-id (racket sandbox-namespace-specs))
    (define make-evaluator-id (racket make-evaluator))
    (define make-module-evaluator-id (racket make-module-evaluator))
    (define module-begin-id (racket #%module-begin))
    (define pretty-print-id (racket pretty-print))
    (define printable<%>-id (racket printable<%>))
    (define gui-dynamic-require-id (racket gui-dynamic-require))))
@(def-extras unit-struct
             unit-struct/ctc
             make-base-namespace-id
             make-base-empty-namespace-id
             sandbox-namespace-specs-id
             make-evaluator-id
             make-module-evaluator-id
             module-begin-id
             pretty-print-id
             printable<%>-id
             gui-dynamic-require-id)

@(define-syntax-rule (compat-except sid rid . rest)
   (begin
     @section[@racketmodname[sid]]
     @defmodule[sid]
     "The " @racketmodname[sid] " library re-exports " @racketmodname[rid] (begin . rest) "."))
@(define-syntax-rule (compat sid rid)
   (compat-except sid rid))

@title{Scheme: Compatibility Libraries and Executables}

Racket was once called ``PLT Scheme,'' and a number of libraries with
names starting @racketidfont{scheme} provide compatibility with the
old name. A few @seclink["compat-exe"]{old executables} are also provided.

Do not use @racketmodfont{#lang} @racketmodname[scheme] to start new projects;
@racketmodfont{#lang} @racketmodname[racket] is the preferred language.

@table-of-contents[]

@compat-except[scheme racket]{, except based on @racketmodname[scheme/base]
instead of @racketmodname[racket/base], the @|unit-struct| and @|unit-struct/ctc| from
@racketmodname[scheme/unit] is exported, @racketmodname[scheme/set] is
not re-exported, @racketmodname[scheme/system] is
not re-exported, @racket[pretty-print] is re-directed in as
@racketmodname[scheme/pretty], and @racketmodname[scheme/nest] is
re-exported}

@compat-except[scheme/base racket/base]{, except that
@racketmodname[racket]'s @racket[struct], @racket[hash],
@racket[hasheq], @racket[hasheqv], @racket[in-directory], and
@racket[local-require] are not exported, and
@racket[make-base-namespace], @racket[make-base-empty-namespace]
@racket[#%module-begin]
are different}

@defproc[(make-base-empty-namespace) namespace?]{

Like @|make-base-empty-namespace-id| from @racketmodname[racket/base],
but with @racketmodname[scheme/base] attached.}

@defproc[(make-base-namespace) namespace?]{

Like @|make-base-namespace-id| from @racketmodname[racket/base], but
with @racketmodname[scheme/base] attached.}

@defform[(#%module-begin form ...)]{

Like @|module-begin-id| from @racketmodname[racket/base], but declares
a @racket[configure-runtime] submodule that uses
@racketmodname[scheme/runtime-config] instead of
@racketmodname[racket/runtime-config], and it does not check for an
immediate declaration of @racket[configure-runtime] among the @racket[form]s.}


@compat[scheme/async-channel racket/async-channel]
@compat[scheme/bool racket/bool]

@; ----------------------------------------------------------------------

@compat-except[scheme/class racket/class]{, except that
@racket[writable<%>] is exported under the name @racket[printable<%>]
(and @|printable<%>-id| from @racketmodname[racket/class] is not
exported)}

@defthing[printable<%> interface?]{

An alias for @racket[writable<%>].
}

@; ----------------------------------------------------------------------

@compat[scheme/cmdline racket/cmdline]
@compat[scheme/contract racket/contract]
@compat[scheme/control racket/control]
@compat[scheme/date racket/date]
@compat[scheme/dict racket/dict]
@; @compat[scheme/fasl racket/fasl]
@compat[scheme/file racket/file]
@compat[scheme/fixnum racket/fixnum]
@compat[scheme/flonum racket/flonum]

@; ----------------------------------------------------------------------

@compat-except[scheme/foreign ffi/unsafe]{,
@racketmodname[ffi/unsafe/cvector], and @racketmodname[ffi/vector],
except that @racket[unsafe!]  must be used to import the unsafe
bindings of @racketmodname[ffi/unsafe] and @racketmodname[ffi/unsafe/cvector]}

@defform[(unsafe!)]{

Makes unsafe bindings available.}


@defform/subs[#:literals (unsafe rename-out)
              (provide* provide-star-spec ...)
              ([provide-star-spec (unsafe id)
                                  (unsafe (rename-out [id external-id]))
                                  provide-spec])]{

Like @racket[provide], but @racket[id]s under @racket[unsafe] are not
actually provided. Instead, they are collected for introduction into
an importing module via a macro created by @racket[define-unsafer].}

@defform[(define-unsafer id)]{

Cooperates with @racket[provide*] to define @racket[id] as a
@racket[unsafe!]-like form that introduces definitions for each
binding provided as @racket[unsafe].  The @racket[define-unsafer] form
must occur after all the @racket[provide*] forms to which it refers.}

@; ----------------------------------------------------------------------

@compat[scheme/function racket/function]
@compat[scheme/future racket/future]
@compat[scheme/generator racket/generator]

@; ----------------------------------------------------------------------

@compat-except[scheme/gui racket/gui]{, except that it builds on
@racketmodname[scheme/gui/base] instead of @racketmodname[racket/gui/base]}

@; ----------------------------------------------------------------------

@compat-except[scheme/gui/base racket/gui/base]{, except that it builds on
@racketmodname[scheme] instead of @racketmodname[racket]}

@defproc[(make-gui-empty-namespace) namespace?]{

Like @racket[make-base-empty-namespace], but with
@racketmodname[scheme/class] and @racketmodname[scheme/gui/base] also
attached to the result namespace.}

@defproc[(make-gui-namespace) namespace?]{

Like @racket[make-base-namespace], but with @racketmodname[scheme/class] and
@racketmodname[scheme/gui/base] also required into the top-level
environment of the result namespace.}

@; ----------------------------------------------------------------------

@compat-except[scheme/gui/dynamic racket/gui/dynamic]{, except
that @racket[gui-dynamic-require] extracts bindings from @racket[mred]
instead of @racket[scheme/gui/base]}

@defproc[(gui-dynamic-require [sym symbol?]) any]{

Like @|gui-dynamic-require-id| from @racket[racket/gui/base], but
to access exports of @racketmodname[scheme/gui/base].}

@; ----------------------------------------------------------------------
@compat[scheme/help racket/help]
@compat[scheme/include racket/include]
@; ----------------------------------------------------------------------

@compat-except[scheme/init racket/init]{, except that it builds on
@racketmodname[scheme] instead pf @racketmodname[racket]}

@;------------------------------------------------------------------------

@section[#:tag "scheme/language-info"]{@racketmodname[scheme/language-info]}

@defmodule[scheme/language-info]{
The @racketmodname[scheme/language-info] library is like
@racketmodname[racket/language-info], except that it produces
@racket['(#(scheme/runtime-config configure #f))] for the
@racket['configure-runtime] information key.}

See also @racketmodname[scheme/runtime-config].

@;------------------------------------------------------------------------

@compat[scheme/list racket/list]
@compat[scheme/load racket/load]
@compat[scheme/local racket/local]
@compat[scheme/match racket/match]
@compat[scheme/math racket/math]
@compat[scheme/mpair compatibility/mlist]

@;------------------------------------------------------------------------
@section[#:tag "nest"]{@racketmodname[scheme/nest]}

@defmodule[scheme/nest]

@defform[(nest ([datum ...+] ...) body ...+)]{

Combines nested expressions that syntactically drift to the right into
a more linear textual format, much in the same way that @racket[let*]
linearizes a sequence of nested @racket[let] expressions.

For example,

@racketblock[
(nest ([let ([x 10]
             [y 6])]
       [with-handlers ([exn:fail? (lambda (x) 15)])]
       [parameterize ([current-output-port (current-error-port)])]
       [let-values ([(d r) (quotient/remainder x y)])])
  (display (+ d r)))
]

is equivalent to

@racketblock[
(let ([x 10]
      [y 6])
  (with-handlers ([exn:fail? (lambda (x) 15)])
    (parameterize ([current-output-port (current-error-port)])
      (let-values ([(d r) (quotient/remainder x y)])
        (display (+ d r))))))
]

The @racket[nest] form is unusual in that it has no semantics apart
from its expansion, and its implementation is easier to understand
than a precise prose description:

@racketblock[
(define-syntax nest
  (syntax-rules ()
    [(nest () body0 body ...)
     (let () body0 body ...)]
    [(nest ([form forms ...]) body0 body ...)
     (form forms ... (let () body0 body ...))]
    [(nest ([form forms ...] . more) body0 body ...)
     (form forms ... (nest more body0 body ...))]))
]}

@; ----------------------------------------

@compat[scheme/package compatibility/package]
@compat[scheme/path racket/path]
@compat[scheme/port racket/port]

@; ----------------------------------------

@compat-except[scheme/pretty racket/pretty]{, except that
@racket[pretty-write] is exported under the name @racket[pretty-print]
(and @|pretty-print-id| from @racketmodname[racket/pretty] is not
exported)}

@defproc[(pretty-print [v any/c] [port output-port? (current-output-port)])
         void?]{

An alias for @racket[pretty-write].}

@; ----------------------------------------

@compat[scheme/promise racket/promise]
@compat[scheme/provide racket/provide]
@compat[scheme/provide-syntax racket/provide-syntax]
@compat[scheme/provide-transform racket/provide-transform]
@compat[scheme/require racket/require]
@compat[scheme/require-syntax racket/require-syntax]
@compat[scheme/require-transform racket/require-transform]

@;------------------------------------------------------------------------

@section[#:tag "scheme/runtime-config"]{@racketmodname[scheme/runtime-config]}

@defmodule[scheme/runtime-config]{
The @racketmodname[scheme/runtime-config] library is like
@racketmodname[racket/runtime-config], except that its
@racketidfont{configure} sets
@racket[print-as-expression] to @racket[#f].}

@; ----------------------------------------

@compat[scheme/runtime-path racket/runtime-path]

@; ----------------------------------------

@compat-except[scheme/sandbox racket/sandbox]{, except that
@|sandbox-namespace-specs-id|, @|make-evaluator-id|, and
@|make-module-evaluator-id| are replaced}

@defparam[sandbox-namespace-specs spec (cons/c (-> namespace?) 
                                               (listof module-path?))]{

Like @|sandbox-namespace-specs-id| from
@racketmodname[racket/sandbox], but the default is @racket[(list
make-base-namespace)] if @racket[gui?] is @racket[#f], @racket[(list
make-gui-namespace)] if @racket[gui?] is @racket[#t].}

@defproc*[([(make-evaluator [language (or/c module-path?
                                            (list/c 'special symbol?)
                                            (cons/c 'begin list?))]
                            [input-program any/c] ...
                            [#:requires requires (listof (or/c module-path? path?))]
                            [#:allow-read allow (listof (or/c module-path? path?))])
            (any/c . -> . any)]
           [(make-module-evaluator [module-decl (or/c syntax? pair?)]
                                   [#:language   lang  (or/c #f module-path?)]
                                   [#:allow-read allow (listof (or/c module-path? path?))])
            (any/c . -> . any)])]{

Like @|make-evaluator-id| and @|make-module-evaluator-id| from
@racketmodname[racket/sandbox], but the value of the
@racket[sandbox-namespace-specs] parameter is installed as the
value of @|sandbox-namespace-specs-id| from
@racketmodname[racket/sandbox] before chaining to @|make-evaluator-id|
and @|make-module-evaluator-id| from @racketmodname[racket/sandbox].}

@; ----------------------------------------

@compat[scheme/serialize racket/serialize]
@compat[scheme/set racket/set]
@compat[scheme/signature racket/signature]
@compat[scheme/shared racket/shared]
@compat[scheme/splicing racket/splicing]
@compat[scheme/string racket/string]
@compat[scheme/struct-info racket/struct-info]
@compat[scheme/stxparam racket/stxparam]
@compat[scheme/stxparam-exptime racket/stxparam-exptime]
@compat[scheme/surrogate racket/surrogate]
@compat[scheme/system racket/system]
@compat[scheme/tcp racket/tcp]
@; @compat[scheme/trace racket/trace]
@compat[scheme/trait racket/trait]
@compat[scheme/udp racket/udp]

@compat-except[scheme/unit racket/unit]{, except that @|unit-struct|
and @|unit-struct/ctc| are @racket[struct~s] and
@racket[struct~s/ctc] from @racketmodname[mzlib/unit] instead of
@racket[struct] from @racket[racket/base] and @racket[struct/ctc] from
@racketmodname[racket/unit]}

@compat[scheme/unit-exptime racket/unit-exptime]
@compat[scheme/unsafe/ops racket/unsafe/ops]
@compat[scheme/vector racket/vector]

@; ----------------------------------------

@section[@racketmodname[mred]]
@defmodule[mred]

The @racketmodname[mred] library is like
@racketmodname[scheme/gui/base], except that it provides variants of
@racket[make-gui-namespace] and @racket[make-gui-empty-namespace] that
attach @racketmodname[mred] instead of
@racketmodname[scheme/gui/base].

Both @racketmodname[scheme/gui/base] and
@racketmodname[racket/gui/base] depend on @racketmodname[mred], so it
is attached by all variants of @racket[make-gui-empty-namespace].

@defmodule*/no-declare[(mred/mred)]

The @racketmodname[mred] library actually just re-exports
@racketmodname[mred/mred], which is an even older name for the
library.

@; ----------------------------------------

@include-section["compat.scrbl"]
