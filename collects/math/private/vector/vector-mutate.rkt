#lang typed/racket/base

(require racket/fixnum
         math/private/unsafe)

(provide vector-swap!
         vector-scale!
         vector-scaled-add!)

(: vector-swap! (All (A) ((Vectorof A) Integer Integer -> Void)))
(define (vector-swap! vs i0 i1)
  (unless (= i0 i1)
    (define tmp (unsafe-vector-ref vs i0))
    (unsafe-vector-set! vs i0 (unsafe-vector-ref vs i1))
    (unsafe-vector-set! vs i1 tmp)))

(define-syntax-rule (vector-generic-scale! vs-expr v-expr *)
  (let* ([vs  vs-expr]
         [v   v-expr]
         [n   (vector-length vs)])
    (let loop ([#{i : Nonnegative-Fixnum} 0])
      (if (i . fx< . n)
          (begin (unsafe-vector-set! vs i (* v (unsafe-vector-ref vs i)))
                 (loop (fx+ i 1)))
          (void)))))

(: vector-scale! (case-> ((Vectorof Real) Real -> Void)
                         ((Vectorof Number) Number -> Void)))
(define (vector-scale! vs v)
  (vector-generic-scale! vs v *))

(define-syntax-rule (vector-generic-scaled-add! vs0-expr vs1-expr v-expr + *)
  (let* ([vs0  vs0-expr]
         [vs1  vs1-expr]
         [v    v-expr]
         [n    (min (vector-length vs0) (vector-length vs1))])
    (let loop ([#{i : Nonnegative-Fixnum} 0])
      (if (i . fx< . n)
          (begin (unsafe-vector-set! vs0 i (+ (unsafe-vector-ref vs0 i)
                                              (* (unsafe-vector-ref vs1 i) v)))
                 (loop (fx+ i 1)))
          (void)))))

(: vector-scaled-add! (case-> ((Vectorof Real) (Vectorof Real) Real -> Void)
                              ((Vectorof Number) (Vectorof Number) Number -> Void)))
(define (vector-scaled-add! v0 v1 s)
  (vector-generic-scaled-add! v0 v1 s + *))
