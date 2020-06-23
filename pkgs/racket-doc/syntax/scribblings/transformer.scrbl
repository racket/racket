#lang scribble/doc
@(require "common.rkt" scribble/eval (for-label syntax/transformer))

@(define the-eval (make-base-eval))
@(the-eval '(require syntax/transformer (for-syntax racket/base syntax/transformer)))

@title[#:tag "syntax/transformer"]{Creating Macro Transformers}

@defmodule[syntax/transformer]

@defproc[(make-variable-like-transformer
          [reference-stx (or/c syntax? (-> identifier? syntax?))]
          [setter-stx (or/c syntax? (-> syntax? syntax?) #f) #f])
         (and/c set!-transformer? (-> syntax? syntax?))]{

Creates a transformer that replaces references to the macro identifier
with @racket[reference-stx]. Uses of the macro in operator position
are interpreted as an application with @racket[reference-stx] as the
function and the arguments as given. If the @racket[reference-stx] is
a procedure, it is applied to the macro identifier.

If the macro identifier is used as the target of a @racket[set!] form,
then the @racket[set!] form expands into the application of
@racket[setter-stx] to the @racket[set!] expression's right-hand side,
if @racket[setter-stx] is syntax; otherwise, the identifier is
considered immutable and a syntax error is raised. If
@racket[setter-stx] is a procedure, it is applied to the entire
@racket[set!] expression.

@examples[#:eval the-eval
(define the-box (box add1))
(define-syntax op
  (make-variable-like-transformer
   #'(unbox the-box)
   #'(lambda (v) (set-box! the-box v))))
(op 5)
(set! op 0)
op
]

@history[#:added "6.3"]{}
}

@close-eval[the-eval]

@defproc[(make-expression-transformer
           [transformer (-> syntax? syntax?)])
         (-> syntax? syntax?)]{

Creates a transformer derived from @racket[transformer] that ensures it expands
in an expression context. When invoked in an expression context, it calls
@racket[transformer]. When invoked in any other context, the new
transformer wraps the argument syntax with @racket[#%expression].

@history[#:added "7.7.0.9"]{}
}
