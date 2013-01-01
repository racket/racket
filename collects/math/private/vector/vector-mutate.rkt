#lang typed/racket/base

(require racket/fixnum
         racket/math
         math/private/unsafe)

(provide vector-swap!
         vector-scale!
         vector-scaled-add!
         vector-mag^2
         vector-dot
         vector-normalize!
         vector-sub-proj!
         vector-zero!
         vector-zero?)

(: mag^2 (Number -> Nonnegative-Real))
(define (mag^2 x)
  (max 0 (real-part (* x (conjugate x)))))

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

(define-syntax-rule (vector-generic-scaled-add! vs0-expr vs1-expr v-expr start-expr + *)
  (let* ([vs0  vs0-expr]
         [vs1  vs1-expr]
         [v    v-expr]
         [n    (fxmin (vector-length vs0) (vector-length vs1))])
    (let loop ([#{i : Nonnegative-Fixnum} (fxmin start-expr n)])
      (if (i . fx< . n)
          (begin (unsafe-vector-set! vs0 i (+ (unsafe-vector-ref vs0 i)
                                              (* (unsafe-vector-ref vs1 i) v)))
                 (loop (fx+ i 1)))
          (void)))))

(: vector-scaled-add! (case-> ((Vectorof Real) (Vectorof Real) Real -> Void)
                              ((Vectorof Real) (Vectorof Real) Real Index -> Void)
                              ((Vectorof Number) (Vectorof Number) Number -> Void)
                              ((Vectorof Number) (Vectorof Number) Number Index -> Void)))
(define (vector-scaled-add! vs0 vs1 s [start 0])
  (vector-generic-scaled-add! vs0 vs1 s start + *))

(: vector-mag^2 (case-> ((Vectorof Real) -> Nonnegative-Real)
                        ((Vectorof Number) -> Nonnegative-Real)))
(define (vector-mag^2 vs)
  (define n (vector-length vs))
  (let loop ([#{i : Nonnegative-Fixnum} 0] [#{s : Nonnegative-Real} 0])
    (if (i . fx>= . n) s (loop (fx+ i 1) (+ s (mag^2 (unsafe-vector-ref vs i)))))))

(: vector-dot (case-> ((Vectorof Real) (Vectorof Real) -> Real)
                      ((Vectorof Number) (Vectorof Number) -> Number)))
(define (vector-dot vs0 vs1)
  (define n (min (vector-length vs0) (vector-length vs1)))
  (cond [(= n 0)  0]
        [else
         (define v0 (unsafe-vector-ref vs0 0))
         (define v1 (unsafe-vector-ref vs1 0))
         (let loop ([#{i : Nonnegative-Fixnum} 1] [s (* v0 (conjugate v1))])
           (cond [(i . fx< . n)
                  (define v0 (unsafe-vector-ref vs0 i))
                  (define v1 (unsafe-vector-ref vs1 i))
                  (loop (fx+ i 1) (+ s (* v0 (conjugate v1))))]
                 [else  s]))]))

(: vector-normalize! (case-> ((Vectorof Real) -> Nonnegative-Real)
                             ((Vectorof Number) -> Nonnegative-Real)))
(define (vector-normalize! vs)
  (define n (vector-length vs))
  (define s (sqrt (vector-mag^2 vs)))
  (unless (and (zero? s) (exact? s))
    (let loop ([#{i : Nonnegative-Fixnum} 0])
      (when (i . fx< . n)
        (unsafe-vector-set! vs i (/ (unsafe-vector-ref vs i) s))
        (loop (fx+ i 1)))))
  s)

(: vector-sub-proj! (case-> ((Vectorof Real) (Vectorof Real) Any -> Nonnegative-Real)
                            ((Vectorof Number) (Vectorof Number) Any -> Nonnegative-Real)))
(define (vector-sub-proj! vs0 vs1 unit?)
  (define n (min (vector-length vs0) (vector-length vs1)))
  (define t (if unit? 1 (vector-mag^2 vs1)))
  (unless (and (zero? t) (exact? t))
    (define s (/ (vector-dot vs0 vs1) t))
    (let loop ([#{i : Nonnegative-Fixnum} 0])
      (when (i . fx< . n)
        (define v0 (unsafe-vector-ref vs0 i))
        (define v1 (unsafe-vector-ref vs1 i))
        (unsafe-vector-set! vs0 i (- v0 (* v1 s)))
        (loop (fx+ i 1)))))
  t)

(: vector-zero! (case-> ((Vectorof Real) -> Void)
                        ((Vectorof Number) -> Void)))
(define (vector-zero! vs)
  (define n (vector-length vs))
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (when (i . fx< . n)
      (unsafe-vector-set! vs i 0)
      (loop (fx+ i 1)))))

(: vector-zero? (case-> ((Vectorof Real) -> Boolean)
                        ((Vectorof Number) -> Boolean)))
(define (vector-zero? vs)
  (define n (vector-length vs))
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (cond [(i . fx>= . n)  #t]
          [(zero? (unsafe-vector-ref vs i))
           (loop (fx+ i 1))]
          [else  #f])))
