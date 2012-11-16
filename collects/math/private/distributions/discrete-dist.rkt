#lang typed/racket/base

(require racket/promise
         "../../flonum.rkt"
         "../statistics/statistics-utils.rkt"
         "impl/walker-table.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide Discrete-Dist
         discrete-dist
         discrete-dist-values
         discrete-dist-weights)

(define-distribution-type: (Discrete-Dist A) (Dist A A) (In Out)
  discrete-dist ([values : (Listof Out)] [weights : (Listof Positive-Flonum)]))

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
    (define random (λ () (walker-table-sample (force table))))
    (make-discrete-dist pdf random xs ws)))
