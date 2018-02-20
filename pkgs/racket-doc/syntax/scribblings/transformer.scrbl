#lang scribble/doc
@(require "common.rkt" scribble/eval (for-label syntax/transformer))

@(define the-eval (make-base-eval))
@(the-eval '(require syntax/transformer (for-syntax racket/base syntax/transformer)))

@title[#:tag "syntax/transformer"]{Creating Macro Transformers}

@defmodule[syntax/transformer]

@defproc[(make-variable-like-transformer [reference-stx syntax?]
                                         [setter-stx (or/c syntax? #f) #f])
         set!-transformer?]{

Creates a transformer that replaces references to the macro identifier
with @racket[reference-stx]. Uses of the macro in operator position
are interpreted as an application with @racket[reference-stx] as the
function and the arguments as given.

If the macro identifier is used as the target of a @racket[set!] form,
then the @racket[set!] form expands into the application of
@racket[setter-stx] to the @racket[set!] expression's right-hand side,
if @racket[setter-stx] is syntax; otherwise, the identifier is
considered immutable and a syntax error is raised.

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

@defproc[(id-transformer [id-trans (-> identifier? syntax?)])
         (-> syntax? syntax?)]{

Creates a transformer for an identifier macro that calls
@racket[id-trans] on the identifier, whether the macro is
used as a bare identifier or within parentheses.

@examples[#:eval the-eval
(code:comment
 "simple example, could be done with make-variable-like-transformer")
(define-syntax my-add1
  (id-transformer
   (lambda (stx) #'add1)))
my-add1
(my-add1 5)
(code:comment
 "more complex example, taking advantage of being a transformer")
(define-syntax whereami
  (id-transformer
   (lambda (stx)
     (unless (even? (syntax-line stx))
       (raise-syntax-error #f "must be used on an even-numbered line" stx))
     (syntax-property #'add1 'line (syntax-line stx)))))
(eval:error whereami)     ; line 11
whereami                  ; line 12
(eval:error (whereami 6)) ; line 13
(whereami 6)              ; line 14
]
}

@close-eval[the-eval]
