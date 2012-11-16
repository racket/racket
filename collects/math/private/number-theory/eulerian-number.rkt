#lang typed/racket/base

(require "types.rkt")

(provide eulerian-number)

(: eulerian-number* : Natural Natural -> Natural)
;   computes the Eulerian number <n,k>
;   http://mathworld.wolfram.com/EulerianNumber.html
(define (eulerian-number* n k)
  ; Implementation note:        
  ;   Uses standard recurrence : <n,k> = (k+1) <n-1,k> + (n-k) <n-1,k-1>
  ;   Time: O(n^2)
  (cond
    [(= k 0) 1]
    [else
     (define: E : (Vectorof Integer)
       (make-vector (max (+ k 1) (+ n 1)) 0))
     (vector-set! E 0 1) ; <0,0> = 1
     (for: ([i : Positive-Integer (in-range 1 (+ n 1))])
       (for: ([j : Integer (in-range (- i 1) 0 -1)])
         (vector-set! E j (+ (* (+ j 1) (vector-ref E j))
                             (* (- i j) (vector-ref E (- j 1)))))))
     (assert (vector-ref E k) natural?)]))

(: eulerian-number (Integer Integer -> Natural))
(define (eulerian-number n k)
  (cond [(n . < . 0)  (raise-argument-error 'eulerian-number "Natural" 0 n k)]
        [(k . < . 0)  (raise-argument-error 'eulerian-number "Natural" 1 n k)]
        [else  (eulerian-number* n k)]))
