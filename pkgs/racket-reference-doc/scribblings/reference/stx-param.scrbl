#lang scribble/doc
@(require "mz.rkt"
          (for-label racket/stxparam racket/stxparam-exptime racket/splicing))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/stxparam
                     (only-in racket/control abort)
                     (for-syntax racket/base)))

@title[#:tag "stxparam"]{Syntax Parameters}

@note-lib-only[racket/stxparam]

@defform[(define-syntax-parameter id expr)]{

Binds @racket[id] as syntax to a @deftech{syntax
parameter}. The @racket[expr] is an expression in the
@tech{transformer environment} that serves as the default value for
the @tech{syntax parameter}. The value is typically obtained by a transformer
using @racket[syntax-parameter-value].

The @racket[id] can be used with @racket[syntax-parameterize]
or @racket[syntax-parameter-value] (in a transformer). If
@racket[expr] produces a procedure of one argument or a
@racket[make-set!-transformer] result, then @racket[id] can be
used as a macro. If @racket[expr] produces a
@racket[make-rename-transformer] result, then @racket[id] can be
used as a macro that expands to a use of the target identifier, but
@racket[syntax-local-value] of @racket[id] does not produce
the target's value.

@examples[#:eval the-eval
  (define-syntax-parameter current-class #f)
  (define-syntax-parameter yield (make-rename-transformer #'abort))
  (define-syntax-parameter define/public
    (λ (stx)
      (raise-syntax-error #f "use of a class keyword not in a class" stx)))
  (begin-for-syntax (displayln (syntax-parameter-value #'current-class)))
  (yield 5)
]}

@defform[(syntax-parameterize ([id expr] ...) body-expr ...+)]{

@margin-note/ref{See also @racket[splicing-syntax-parameterize].}

Each @racket[id] must be bound to a @tech{syntax parameter} using
@racket[define-syntax-parameter]. Each @racket[expr] is an expression
in the @tech{transformer environment}. During the expansion of the
@racket[body-expr]s, the value of each @racket[expr] is bound to the
corresponding @racket[id].

If an @racket[expr] produces a procedure of one argument or a
@racket[make-set!-transformer] result, then its @racket[id]
can be used as a macro during the expansion of the
@racket[body-expr]s. If @racket[expr] produces a
@racket[make-rename-transformer] result, then @racket[id] can be
used as a macro that expands to a use of the target identifier, but
@racket[syntax-local-value] of @racket[id] does not produce
the target's value.

@examples[#:eval the-eval
(define-syntax-parameter abort (syntax-rules ()))

(define-syntax forever
  (syntax-rules ()
    [(forever body ...)
     (call/cc (lambda (abort-k)
       (syntax-parameterize
           ([abort (syntax-rules () [(_) (abort-k)])])
         (let loop () body ... (loop)))))]))

(define-syntax-parameter it (syntax-rules ()))

(define-syntax aif
  (syntax-rules ()
    [(aif test then else)
     (let ([t test])
       (syntax-parameterize ([it (syntax-id-rules () [_ t])])
         (if t then else)))]))
]}

@defform[(define-rename-transformer-parameter id expr)]{

Binds @racket[id] as syntax to a @tech{syntax parameter} that must
be bound to a @racket[make-rename-transformer] result and, unlike
@racket[define-syntax-parameter], @racket[syntax-local-value] of
@racket[id] @emph{does} produce the target's value, including inside
of @racket[syntax-parameterize].

@examples[#:eval the-eval #:escape UNSYNTAX
 (define-syntax (test stx)
  (syntax-case stx ()
    [(_ t)
     #`#,(syntax-local-value #'t)]))
 (define-syntax one 1)
 (define-syntax two 2)
 (define-syntax-parameter not-num
   (make-rename-transformer #'one))
 (test not-num)

 (define-rename-transformer-parameter num
   (make-rename-transformer #'one))
 (test num)
 (syntax-parameterize ([num (make-rename-transformer #'two)])
   (test num))
]

@history[#:added "6.3.0.14"]}

@; ----------------------------------------------------------------------

@section{Syntax Parameter Inspection}

@defmodule*/no-declare[(racket/stxparam-exptime)]

@declare-exporting[racket/stxparam-exptime racket/stxparam]

@defproc[(syntax-parameter-value [id-stx syntax?]) any]{

This procedure is intended for use in a @tech{transformer
environment}, where @racket[id-stx] is an identifier bound in the
normal environment to a @tech{syntax parameter}. The result is the current
value of the @tech{syntax parameter}, as adjusted by
@racket[syntax-parameterize] form.

This binding is provided @racket[for-syntax] by
@racketmodname[racket/stxparam], since it is normally used in a
transformer. It is provided normally by
@racketmodname[racket/stxparam-exptime].}


@defproc[(make-parameter-rename-transformer [id-stx syntax?]) any]{

This procedure is intended for use in a transformer, where
@racket[id-stx] is an identifier bound to a @tech{syntax parameter}. The
result is a transformer that behaves as @racket[id-stx], but that cannot
be used with @racket[syntax-parameterize] or
@racket[syntax-parameter-value].

Using @racket[make-parameter-rename-transformer] is analogous to
defining a procedure that calls a parameter. Such a procedure can be
exported to others to allow access to the parameter value, but not to
change the parameter value. Similarly,
@racket[make-parameter-rename-transformer] allows a @tech{syntax parameter}
to be used as a macro, but not changed.

The result of @racket[make-parameter-rename-transformer] is not
treated specially by @racket[syntax-local-value], unlike the result
of @racket[make-rename-transformer].

This binding is provided @racket[for-syntax] by
@racketmodname[racket/stxparam], since it is normally used in a
transformer. It is provided normally by
@racketmodname[racket/stxparam-exptime].}

@(close-eval the-eval)
