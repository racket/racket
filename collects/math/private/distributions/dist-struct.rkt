#lang racket/base

(provide
 PDF CDF Inverse-CDF
 Dist
 Ordered-Dist
 Real-Dist
 Real-PDF
 Real-CDF
 Real-Inverse-CDF
 dist?
 ordered-dist?
 dist-pdf
 dist-random
 dist-cdf
 dist-inv-cdf
 dist-min
 dist-max
 dist-median
 sample
 sample*
 real-dist-prob)

(module typed-defs typed/racket/base
  (require racket/performance-hint
           racket/promise
           "../../flonum.rkt")
  
  (provide (all-defined-out))
  
  (define-type (PDF In)
    (case-> (In -> Flonum)
            (In Any -> Flonum)))
  
  (define-type (CDF In)
    (case-> (In -> Flonum)
            (In Any -> Flonum)
            (In Any Any -> Flonum)))
  
  (define-type (Inverse-CDF Out)
    (case-> (Real -> Out)
            (Real Any -> Out)
            (Real Any Any -> Out))) 
  
  (struct: (In Out) Dist ([pdf : (PDF In)]
                          [random : (-> Out)])
    #:transparent)
  
  (struct: (In Out) Ordered-Dist Dist
    ([cdf : (CDF In)]
     [inv-cdf : (Inverse-CDF Out)]
     [min : Out]
     [max : Out]
     [median : (Promise Out)])
    #:transparent)
  
  (define-type Real-Dist (Ordered-Dist Real Flonum))
  (define-type Real-PDF (PDF Real))
  (define-type Real-CDF (CDF Real))
  (define-type Real-Inverse-CDF (Inverse-CDF Flonum))
  
  ;; =================================================================================================
  
  (begin-encourage-inline
    
    (: dist-median (All (In Out) ((Ordered-Dist In Out) -> Out)))
    (define (dist-median d) (force (Ordered-Dist-median d)))
    
    (: sample (All (In Out) ((Dist In Out) -> Out)))
    (define (sample d) ((Dist-random d)))
    
    (: sample* (All (In Out) ((Dist In Out) Integer -> (Listof Out))))
    (define (sample* d n)
      (cond [(n . < . 0)  (raise-argument-error 'sample* "Natural" 1 d n)]
            [(index? n)  (define random (Dist-random d))
                         (for/list: : (Listof Out) ([_  (in-range n)]) (random))]
            [else  (raise-argument-error 'sample* "Index" 1 d n)]))
    
    )
  
  (: real-dist-prob (case-> (Real-Dist Real Real -> Flonum)
                            (Real-Dist Real Real Any -> Flonum)
                            (Real-Dist Real Real Any Any -> Flonum)))
  (define (real-dist-prob d a b [log? #f] [1-p? #f])
    (let ([a  (fl a)] [b  (fl b)])
      (let ([a  (flmin a b)] [b  (flmax a b)])
        (define c (force (Ordered-Dist-median d)))
        (define cdf (Ordered-Dist-cdf d))
        (define log-p
          (min (cond [1-p?  (lg+ (cdf a #t #f) (cdf b #t #t))]
                     [(b . fl<= . c)
                      (define log-P_x<=a (cdf a #t #f))
                      (define log-P_x<=b (cdf b #t #f))
                      (cond [(log-P_x<=b . fl< . log-P_x<=a)  -inf.0]
                            [else  (lg- log-P_x<=b log-P_x<=a)])]
                     [(a . fl>= . c)
                      (define log-P_x>a (cdf a #t #t))
                      (define log-P_x>b (cdf b #t #t))
                      (cond [(log-P_x>a . fl< . log-P_x>b)  -inf.0]
                            [else  (lg- log-P_x>a log-P_x>b)])]
                     [else
                      (define log-P_x<=a (cdf a #t #f))
                      (define log-P_x>b (cdf b #t #t))
                      (define log-P_a<x<=0.5
                        (cond [((fllog 0.5) . fl< . log-P_x<=a)  -inf.0]
                              [else  (lg- (fllog 0.5) log-P_x<=a)]))
                      (define log-P_0.5<x<=b
                        (cond [((fllog 0.5) . fl< . log-P_x>b)  -inf.0]
                              [else  (lg- (fllog 0.5) log-P_x>b)]))
                      (lg+ log-P_a<x<=0.5 log-P_0.5<x<=b)])
               0.0))
        (if log? log-p (flexp log-p)))))

  )

(require (submod "." typed-defs)
         (for-syntax racket/base))

(define-syntax dist? (make-rename-transformer #'Dist?))
(define-syntax ordered-dist? (make-rename-transformer #'Ordered-Dist?))
(define-syntax dist-pdf (make-rename-transformer #'Dist-pdf))
(define-syntax dist-random (make-rename-transformer #'Dist-random))
(define-syntax dist-cdf (make-rename-transformer #'Ordered-Dist-cdf))
(define-syntax dist-inv-cdf (make-rename-transformer #'Ordered-Dist-inv-cdf))
(define-syntax dist-min (make-rename-transformer #'Ordered-Dist-min))
(define-syntax dist-max (make-rename-transformer #'Ordered-Dist-max))
