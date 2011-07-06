#;
(
TR opt: bounds-check.rkt 16:2 (vector-ref v i) -- vector access splitting
TR opt: bounds-check.rkt 19:2 (vector-set! v i n) -- vector access splitting
TR opt: bounds-check.rkt 22:2 (vector-ref v i) -- vector access splitting
TR opt: bounds-check.rkt 25:2 (vector-set! v i n) -- vector access splitting
3
4
5
)

#lang typed/racket

(: f (All (X) ((Vectorof X) Fixnum -> X)))
(define (f v i)
  (vector-ref v i))
(: g (All (X) ((Vectorof X) Fixnum X -> Void)))
(define (g v i n)
  (vector-set! v i n))
(: h (All (X) ((Vectorof X) Index -> X)))
(define (h v i)
  (vector-ref v i))
(: a (All (X) ((Vectorof X) Index X -> Void)))
(define (a v i n)
  (vector-set! v i n))

(define: v : (Vectorof Integer) (vector 1 2 3 4))
(displayln (f v 2))
(g v 2 4)
(displayln (h v 2))
(a v 2 5)
(displayln (f v 2))
