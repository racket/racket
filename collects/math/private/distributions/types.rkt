#lang typed/racket/base

(require racket/performance-hint
         "../../types.rkt")

(provide
 ;; Real-valued distributions
 Real-Density-Function
 Real-Distribution-Function
 Real-Distribution real-pdf real-cdf real-inv-cdf real-random random-real
 ;; Integer-valued distributions
 Integer-Density-Function
 Integer-Distribution-Function
 Integer-Inverse-Distribution-Function
 Integer-Distribution int-pdf int-cdf int-inv-cdf int-random random-int)

;; ===================================================================================================
;; Real-valued distributions

(define-type Real-Density-Function
  (case-> (Real -> Float)
          (Real Any -> Float)))

(define-type Real-Distribution-Function
  (case-> (Real -> Float)
          (Real Any -> Float)
          (Real Any Any -> Float)))

(struct: Real-Distribution ([pdf : Real-Density-Function]
                            [cdf : Real-Distribution-Function]
                            [inv-cdf : Real-Distribution-Function]
                            [random : (-> Float)])
  #:transparent)

(begin-encourage-inline
  
  (define real-pdf Real-Distribution-pdf)
  (define real-cdf Real-Distribution-cdf)
  (define real-inv-cdf Real-Distribution-inv-cdf)
  (define real-random Real-Distribution-random)
  
  (: random-real (Real-Distribution -> Float))
  (define (random-real d) ((real-random d)))
  
  )

;; ===================================================================================================
;; Integer-valued distributions

(define-type Integer-Density-Function
  (case-> (Extended-Integer -> Float)
          (Extended-Integer Any -> Float)))

(define-type Integer-Distribution-Function
  (case-> (Extended-Integer -> Float)
          (Extended-Integer Any -> Float)
          (Extended-Integer Any Any -> Float)))

(define-type Integer-Inverse-Distribution-Function
  (case-> (Real -> Extended-Integer)
          (Real Any -> Extended-Integer)
          (Real Any Any -> Extended-Integer)))

(struct: Integer-Distribution ([pdf : Integer-Density-Function]
                               [cdf : Integer-Distribution-Function]
                               [inv-cdf : Integer-Inverse-Distribution-Function]
                               [random : (-> Integer)])
  #:transparent)

(begin-encourage-inline
  
  (define int-pdf Integer-Distribution-pdf)
  (define int-cdf Integer-Distribution-cdf)
  (define int-inv-cdf Integer-Distribution-inv-cdf)
  (define int-random Integer-Distribution-random)
  
  (: random-int (Integer-Distribution -> Integer))
  (define (random-int d) ((int-random d)))
  
  )
