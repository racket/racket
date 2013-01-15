#lang racket/base

(require (for-syntax racket/base)
         (only-in typed/racket/base assert index?)
         "array-struct.rkt"
         "array-pointwise.rkt"
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

(define-syntax-rule (array-count f arr ...)
  (assert
   (parameterize ([array-strictness #f])
     (array-all-sum (inline-array-map (λ (b) (if b 1 0))
                                      (array-map f arr ...))
                    0))
   index?))

(define-syntax-rule (array-andmap pred? arr ...)
  (parameterize ([array-strictness #f])
    (array-all-and (array-map pred? arr ...))))

(define-syntax-rule (array-ormap pred? arr ...)
  (parameterize ([array-strictness #f])
    (array-all-or (array-map pred? arr ...))))

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
         array-count
         array-andmap
         array-ormap
         array-axis-reduce
         unsafe-array-axis-reduce
         array->list-array)
