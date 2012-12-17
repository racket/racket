#lang typed/racket/base

(require racket/flonum
         "../../base.rkt"
         "../vector/vector.rkt"
         "types.rkt")

(provide partitions)

(define num-global-ps 200)
(: global-ps (Vectorof Natural))
(define global-ps (make-vector num-global-ps 0))
(vector-set! global-ps 0 1)

(: partitions : Integer -> Natural)
; http://en.wikipedia.org/wiki/Partition_(number_theory)
(define (partitions n)
  (define: local-ps : (Vectorof Natural)
    (make-vector (max 0 (- (+ n 1) num-global-ps)) 0))
  
  (: ps-ref! (Integer (-> Natural) -> Natural))
  (define (ps-ref! n thnk)
    (cond [(n . < . num-global-ps)
           (vector-ref! global-ps n thnk exact-zero?)]
          [else
           (vector-ref! local-ps (- n num-global-ps) thnk exact-zero?)]))
  
  (let: loop : Natural ([n : Integer  n])
    (cond [(< n 0)  0]
          [(= n 0)  1]
          [else
           (ps-ref!
            n (λ ()
                (define m (/ (+ 1.0 (flsqrt (+ 1.0 (* 24.0 n)))) 6.0))
                (assert
                 (for/fold: ([sum : Integer  0]) ([k  (in-range 1 (add1 (exact-floor m)))])
                   (+ sum (* (if (odd? k) 1 -1)
                             (+ (loop (- n (quotient (* k (- (* 3 k) 1)) 2)))
                                (loop (- n (quotient (* k (+ (* 3 k) 1)) 2)))))))
                 natural?)))])))
