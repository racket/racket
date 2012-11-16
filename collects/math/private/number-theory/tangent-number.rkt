#lang typed/racket/base

(provide tangent-number)

(: tangent-number : Integer -> Natural)
;  The n'th tangent number:
;  <http://mathworld.wolfram.com/TangentNumber.html>
(define (tangent-number n)
  (cond [(n . < . 0)  (raise-argument-error 'tangent-number "Natural" n)]
        [else
         ; Implementation note:
         ;   See "Concrete Mathematics" p 287 for the method
         (define: T : (Vectorof Natural) (make-vector (+ n 2) 0))
         ; T[x]=x
         (vector-set! T 1 1)
         (for: ([k : Natural (in-range (+ n 1))])
           ; differentiate T[x]
           (for: ([i : Natural (in-range (+ k 1))])
             (vector-set! T i (* (add1 i) (vector-ref T (add1 i)))))
           (vector-set! T k 0)
           ; multiply T[x] with 1+x^2
           (for: ([i : Integer (in-range (+ k 1) 1 -1)])
             (vector-set! T i (+ (vector-ref T i) (vector-ref T (- i 2))))))
         (vector-ref T 0)]))

