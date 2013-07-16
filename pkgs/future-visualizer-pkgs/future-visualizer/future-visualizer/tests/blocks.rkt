#lang scheme/base

(require scheme/future 
         scheme/list 
         rackunit
         (only-in future-visualizer/trace trace-futures) 
         (only-in future-visualizer/private/visualizer-data runtime-block-event?))

;;Test whether a futures program hits any barricades
(define-syntax-rule (blocks? e ...) 
  (let ([log (trace-futures e ...)])
    (> (length (filter runtime-block-event? log)) 0)))

;;Stress test for odd?/even?
(define N 1000)
(define MAX 1000)
(define (rnd x) 
  (case (random 3) 
    [(0)
     (define n (random MAX)) 
     (case (random 2) 
       [(0) (- 0 n)] ;negative
       [(1) n])] ;positive
    [(1) ;float
     (+ (random MAX) .0)]
    [(2) ;bignum
     (expt 2 (+ (random MAX) 65))]))
  
(define (test-even-odd)
  (define ns (build-list N rnd))
  (define fs (for/list ([n (in-list ns)]) 
               (future (λ () 
                          (or (odd? n) (even? n))))))
  (map touch fs))

(void (test-even-odd))

;; Only test for non-blocking when actually running parallel futures
(when (futures-enabled?)
  (check-false (blocks? (void (test-even-odd)))))
