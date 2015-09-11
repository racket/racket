#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt"
          (for-label syntax/parse/define))

@(define the-eval (make-sp-eval))

@title{Defining Simple Macros}

@defmodule[syntax/parse/define]

@defform[(define-simple-macro (macro-id . pattern) pattern-directive ...
           template)]{

Defines a macro named @racket[macro-id]; equivalent to the following:

@racketblock[
(define-syntax (macro-id stx)
  (syntax-parse stx
    [(macro-id . pattern) pattern-directive ... (syntax template)]))
]

@(the-eval '(require syntax/parse/define))
@examples[#:eval the-eval
(define-simple-macro (fn x:id rhs:expr) (lambda (x) rhs))
((fn x x) 17)
(fn 1 2)

(define-simple-macro (fn2 x y rhs)
  #:declare x id
  #:declare y id
  #:declare rhs expr
  (lambda (x y) rhs))
((fn2 a b (+ a b)) 3 4)
(fn2 a #:b 'c)
]

}

@defform[(define-syntax-parser macro-id parse-option ... clause ...+)]{

Defines a macro named @racket[macro-id]; equivalent to:

@racketblock[
(define-syntax macro-id
  (syntax-parser parse-option ... clause ...))
]

@examples[#:eval the-eval
(define-syntax-parser fn3
  [(fn3 x:id rhs:expr)
   #'(lambda (x) rhs)]
  [(fn3 x:id y:id rhs:expr)
   #'(lambda (x y) rhs)])
((fn3 x x) 17)
((fn3 a b (+ a b)) 3 4)
(fn3 1 2)
(fn3 a #:b 'c)
]}

@(close-eval the-eval)
