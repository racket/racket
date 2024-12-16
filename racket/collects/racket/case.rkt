#lang racket/base
(require (for-syntax racket/base)
         (submod "private/case.rkt" make-case))

(provide (rename-out [case case/equal])
         case/equal-always
         case/eq
         case/eqv

         case*
         (rename-out [case* case*/equal])
         case*/equal-always
         case*/eq
         case*/eqv)

(define-syntax case/equal-always
  (make-case #'equal-always? #'quote))
(define-syntax case/eq
  (make-case #'eq? #'quote))
(define-syntax case/eqv
  (make-case #'eqv? #'quote))

(define-syntax case*
  (make-case #'equal? #'quasiquote))
(define-syntax case*/equal-always
  (make-case #'equal-always? #'quasiquote))
(define-syntax case*/eq
  (make-case #'eq? #'quasiquote))
(define-syntax case*/eqv
  (make-case #'eqv? #'quasiquote))
