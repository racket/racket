#lang typed/scheme

(define-type (T elem)
  (U 'other a:empty))

(define-predicate a:list? (T Any))

(define-struct: a:empty ())
