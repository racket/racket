#lang typed/racket
(provide tangent-number)

(: tangent-number : Natural -> Natural)
;  The n'th tangent number:
;  <http://mathworld.wolfram.com/TangentNumber.html>
(define (tangent-number n)
  ; Implementation note:
  ;   See "Concrete Mathematics" p 287 for the method
  (define: T : (Vectorof Natural) (make-vector (+ n 2) 0))
  ; T[x]=x
  (vector-set! T 1 1)
  (for: ([k : Natural (in-range (+ n 1))])
    ; differentiate T[x]
    (for: ([i : Natural (in-range (+ k 1))])
      ((inst vector-set! Natural) T i (* (add1 i) (vector-ref T (add1 i)))))
    ((inst vector-set! Natural) T k 0)
    ; multiply T[x] with 1+x^2
    (for: ([i : Integer (in-range (+ k 1) 1 -1)])
      ((inst vector-set! Natural) T i (+ (vector-ref T i) (vector-ref T (- i 2))))))
  (vector-ref T 0))

