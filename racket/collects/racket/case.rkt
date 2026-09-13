#lang racket/base
(require (for-syntax racket/base)
         (submod "private/case.rkt" make-case))

(provide case/equal
         case/equal-always
         case/eq
         case/eqv)

;; equivalent to `case`:
(define-syntax case/equal
  (make-case #'equal?))

(define-syntax case/equal-always
  (make-case #'equal-always?))
(define-syntax case/eq
  (make-case #'eq?))
(define-syntax case/eqv
  (make-case #'eqv?))
