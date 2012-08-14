#lang typed/racket/base

(require racket/flonum
         "unsafe.rkt")

(provide (all-defined-out))

(define-predicate nonnegative-fixnum? Nonnegative-Fixnum)
(define-predicate listof-nonnegative-fixnum? (Listof Nonnegative-Fixnum))
