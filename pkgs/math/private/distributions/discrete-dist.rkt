#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "../statistics/statistics-utils.rkt"
         "../utils.rkt"
         "impl/walker-table.rkt"
         "dist-struct.rkt")

(provide Discrete-Dist
         discrete-dist
         discrete-dist-values
         discrete-dist-probs)

(begin-encourage-inline
  (struct: (In Out) discrete-dist-struct distribution ([values : (Listof Out)]
                                                       [probs : (Listof Positive-Flonum)])
    #:property prop:custom-print-quotable 'never
    #:property prop:custom-write
    (λ (v port mode)
      (pretty-print-constructor
       'discrete-dist (list (discrete-dist-struct-values v) (discrete-dist-struct-probs v))
       port mode)))
  
  (define-type (Discrete-Dist A) (discrete-dist-struct A A))

  (: discrete-dist-values (All (A) ((Discrete-Dist A) -> (Listof A))))
  (define (discrete-dist-values d) (discrete-dist-struct-values d))
  
  (: discrete-dist-probs (All (A) ((Discrete-Dist A) -> (Listof Positive-Flonum))))
  (define (discrete-dist-probs d) (discrete-dist-struct-probs d))
  
  )

(: discrete-dist
   (All (A) (case-> ((Sequenceof A) -> (Discrete-Dist A))
                    ((Sequenceof A) (Option (Sequenceof Real)) -> (Discrete-Dist A)))))
(define (discrete-dist xs [ws #f])
  (let-values ([(xs ws)  (cond [ws  (sequences->normalized-weighted-samples 'discrete-dist xs ws)]
                               [else  (sequence->normalized-weighted-samples 'discrete-dist xs)])])
    (define hash
      (delay (make-hash ((inst map (Pair A Positive-Flonum) A Positive-Flonum) cons xs ws))))
    
    (define table
      (delay (make-walker-table xs ws)))
    
    (define pdf (opt-lambda: ([x : A] [log? : Any #f])
                  (define p (hash-ref (force hash) x (λ () 0.0)))
                  (if log? (fllog p) p)))
    (define sample (case-lambda:
                     [()  (walker-table-sample (force table))]
                     [([n : Integer])
                      (cond [(n . < . 0)  (raise-argument-error 'discrete-dist-sample "Natural" n)]
                            [else  (let ([table  (force table)])
                                     (build-list n (λ (_) (walker-table-sample table))))])]))
    (discrete-dist-struct pdf sample xs ws)))
