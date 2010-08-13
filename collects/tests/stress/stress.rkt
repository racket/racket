#lang racket
(provide stress)

(define-syntax-rule (stress trials-expr [label body ...] ...)
  (stress* trials-expr
           (cons label (λ () body ...))
           ...))

(define (cumulative-average ca x i)
  (+ ca (/ (- x ca) (add1 i))))

(define (run-experiment how-many exp)
  (match-define (cons label thunk) exp)
  (define-values
    (cpu real gc)
    (for/fold ([cpu0 0.0]
               [real0 0.0]
               [gc0 0.0])
      ([trial-n (in-range how-many)])
      (define exp-cust (make-custodian))
      (define-values (_ cpu1 real1 gc1) 
        (parameterize ([current-custodian exp-cust])
          (time-apply thunk empty)))
      (custodian-shutdown-all exp-cust)
      (when (zero? (modulo trial-n 5))
        (collect-garbage) (collect-garbage))
      (values (cumulative-average cpu0 cpu1 trial-n)
              (cumulative-average real0 real1 trial-n)
              (cumulative-average gc0 gc1 trial-n))))
  (vector label cpu real gc))

(define (stress* how-many . experiments)
  (stress-display
   how-many
   (sort 
    (for/list ([exp (in-list experiments)])
      (run-experiment how-many exp))
    <=
    #:key (λ (v) (vector-ref v 1)))))

(define (stress-display how-many res)
  (for ([v (in-list res)])
    (match-define (vector label cpu real gc) v)
    (printf "~a: cpu time: ~a real time: ~a gc time: ~a (averaged over ~a runs)\n"
            label cpu real gc how-many))
  (newline))