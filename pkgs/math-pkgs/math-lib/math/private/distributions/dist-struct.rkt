#lang typed/racket/base

(require racket/performance-hint)

(provide
 PDF Sample CDF Inverse-CDF
 (struct-out distribution)
 (struct-out ordered-dist)
 Real-Dist
 pdf sample cdf inv-cdf)

(define-type (PDF In)
  (case-> (In -> Flonum)
          (In Any -> Flonum)))

(define-type (Sample Out)
  (case-> (-> Out)
          (Integer -> (Listof Out))))

(define-type (CDF In)
  (case-> (In -> Flonum)
          (In Any -> Flonum)
          (In Any Any -> Flonum)))

(define-type (Inverse-CDF Out)
  (case-> (Real -> Out)
          (Real Any -> Out)
          (Real Any Any -> Out))) 

(struct: (In Out) distribution ([pdf : (PDF In)]
                                [sample : (Sample Out)])
  #:transparent)

(struct: (In Out) ordered-dist distribution
  ([cdf : (CDF In)]
   [inv-cdf : (Inverse-CDF Out)]
   [min : Out]
   [max : Out]
   [median : (Promise Out)])
  #:transparent)

(define-type Real-Dist (ordered-dist Real Flonum))

;; =================================================================================================

(begin-encourage-inline
  
  (: pdf (All (In Out) (case-> ((distribution In Out) In -> Flonum)
                               ((distribution In Out) In Any -> Flonum))))
  (define (pdf d v [log? #f])
    ((distribution-pdf d) v log?))
  
  (: sample (All (In Out) (case-> ((distribution In Out) -> Out)
                                  ((distribution In Out) Integer -> (Listof Out)))))
  (define sample
    (case-lambda
      [(d)  ((distribution-sample d))]
      [(d n)  ((distribution-sample d) n)]))
  
  (: cdf (All (In Out) (case-> ((ordered-dist In Out) In -> Flonum)
                               ((ordered-dist In Out) In Any -> Flonum)
                               ((ordered-dist In Out) In Any Any -> Flonum))))
  (define (cdf d v [log? #f] [1-p? #f])
    ((ordered-dist-cdf d) v log? 1-p?))
  
  (: inv-cdf (All (In Out) (case-> ((ordered-dist In Out) Real -> Out)
                                   ((ordered-dist In Out) Real Any -> Out)
                                   ((ordered-dist In Out) Real Any Any -> Out))))
  (define (inv-cdf d p [log? #f] [1-p? #f])
    ((ordered-dist-inv-cdf d) p log? 1-p?))
  
  )
