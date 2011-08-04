#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt"
          (for-label syntax/parse/define))

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
@myexamples[
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
