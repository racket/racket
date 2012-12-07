#lang racket/base

(require (for-syntax racket/base)
         "typed-array-fold.rkt")

;; ===================================================================================================
;; Standard folds

(define-syntax-rule (define-axis-fold name f)
  (define-syntax (name stx)
    (syntax-case stx ()
      [(_ arr k)  (syntax/loc stx (array-axis-fold arr k f))]
      [(_ arr k init)  (syntax/loc stx (array-axis-fold arr k f init))])))

(define-syntax-rule (define-all-fold name f)
  (define-syntax (name stx)
    (syntax-case stx ()
      [(_ arr)  (syntax/loc stx (array-all-fold arr f))]
      [(_ arr init)  (syntax/loc stx (array-all-fold arr f init))])))

(define-axis-fold array-axis-sum +)
(define-axis-fold array-axis-prod *)
(define-axis-fold array-axis-min min)
(define-axis-fold array-axis-max max)

(define-all-fold array-all-sum +)
(define-all-fold array-all-prod *)
(define-all-fold array-all-min min)
(define-all-fold array-all-max max)

(provide array-axis-fold
         array-axis-sum
         array-axis-prod
         array-axis-min
         array-axis-max
         array-axis-count
         array-axis-and
         array-axis-or
         array-fold
         array-all-fold
         array-all-sum
         array-all-prod
         array-all-min
         array-all-max
         array-all-and
         array-all-or
         array-axis-reduce
         unsafe-array-axis-reduce
         array->list-array)
