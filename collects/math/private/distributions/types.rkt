#lang typed/racket/base

(require racket/performance-hint
         "../../types.rkt")

(provide
 ;; Real-valued distributions
 Real-Density-Function
 Real-Distribution-Function
 Real-Distribution real-pdf real-cdf real-inv-cdf real-random random-real)

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
