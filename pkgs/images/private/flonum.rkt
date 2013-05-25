#lang typed/racket/base

(require (for-syntax typed/racket/base)
         (rename-in racket/flonum
                    [flvector-ref old:flvector-ref]
                    [flvector-set! old:flvector-set!])
         (except-in racket/fixnum fl->fx fx->fl)  ; these two functions are untyped
         racket/math
         (only-in racket/unsafe/ops
                  unsafe-flvector-set! unsafe-flvector-ref
                  unsafe-vector-set! unsafe-vector-ref
                  unsafe-fx+)
         racket/performance-hint)

(provide (all-defined-out)
         (except-out (all-from-out racket/flonum
                                   racket/fixnum)
                     old:flvector-ref
                     old:flvector-set!))

(define-predicate nonnegative-fixnum? Nonnegative-Fixnum)

;; This looks stupid, but it avoids an optimization TR does that is actually a pessimization, by
;; keeping it from recognizing flvector-ref
(: flvector-ref (FlVector Integer -> Float))
(define flvector-ref old:flvector-ref)

;; Ditto above
(: flvector-set! (FlVector Integer Float -> Void))
(define flvector-set! old:flvector-set!)

(define-syntax-rule (inline-build-flvector size f)
  (let: ([n : Integer  size])
    (with-asserts ([n  nonnegative-fixnum?])
      (define vs (make-flvector n))
      (let: loop : FlVector ([i : Nonnegative-Fixnum  0])
        (cond [(i . fx< . n)  (unsafe-flvector-set! vs i (f i))
                              (loop (unsafe-fx+ i 1))]
              [else  vs])))))

(: flvector->vector (FlVector -> (Vectorof Float)))
(define (flvector->vector vs)
  (define n (flvector-length vs))
  (define new-vs (make-vector n 0.0))
  (let: loop : (Vectorof Float) ([k : Nonnegative-Fixnum  0])
    (cond [(k . < . n)  (unsafe-vector-set! new-vs k (unsafe-flvector-ref vs k))
                        (loop (unsafe-fx+ k 1))]
          [else  new-vs])))

(: real-vector->flvector ((Vectorof Real) -> FlVector))
(define (real-vector->flvector vs)
  (define n (vector-length vs))
  (define new-vs (make-flvector n 0.0))
  (let: loop : FlVector ([k : Nonnegative-Fixnum  0])
    (cond [(k . < . n)
           (unsafe-flvector-set! new-vs k (real->double-flonum (unsafe-vector-ref vs k)))
           (loop (unsafe-fx+ k 1))]
          [else  new-vs])))

(begin-encourage-inline
  
  (: ->flvector ((U (Vectorof Real) FlVector) -> FlVector))
  (define (->flvector vs)
    (cond [(flvector? vs)  vs]
          [else  (real-vector->flvector vs)]))
  
  (: fx->fl (Fixnum -> Float))
  (define fx->fl ->fl)
  
  (: fl->fx (Float -> Fixnum))
  (define (fl->fx x)
    (define i (fl->exact-integer x))
    (with-asserts ([i fixnum?]) i))
  
  (: flrational? (Float -> Boolean))
  (define (flrational? x)
    ;; if x = +nan.0, both tests return #f
    (and (x . > . -inf.0) (x . < . +inf.0)))
  
  (: fl-convex-combination (Float Float Float -> Float))
  (define (fl-convex-combination dv sv sa)
    (+ (* sv sa) (* dv (- 1.0 sa))))
  
  (: fl-alpha-blend (Float Float Float -> Float))
  (define (fl-alpha-blend dca sca sa)
    (+ sca (* dca (- 1.0 sa))))
  
  (: flgaussian (Float Float -> Float))
  (define (flgaussian x s)
    (define x/s (/ x s))
    (/ (exp (* -0.5 (* x/s x/s)))
       (* (sqrt (* 2.0 pi)) s)))
  
  (: flsigmoid (Float -> Float))
  (define (flsigmoid x)
    (/ 1.0 (+ 1.0 (exp (- x)))))
  
  ;; =================================================================================================
  ;; 3-vectors
  
  (: fl3dot (Float Float Float Float Float Float -> Float))
  (define (fl3dot x1 y1 z1 x2 y2 z2)
    (+ (* x1 x2) (* y1 y2) (* z1 z2)))
  
  (: fl3* (case-> (Float Float Float Float -> (values Float Float Float))
                  (Float Float Float Float Float Float -> (values Float Float Float))))
  (define fl3*
    (case-lambda
      [(x y z c)  (values (* x c) (* y c) (* z c))]
      [(x1 y1 z1 x2 y2 z2)  (values (* x1 x2) (* y1 y2) (* z1 z2))]))
  
  (: fl3+ (Float Float Float Float Float Float -> (values Float Float Float)))
  (define (fl3+ x1 y1 z1 x2 y2 z2)
    (values (+ x1 x2) (+ y1 y2) (+ z1 z2)))
  
  (: fl3- (case-> (Float Float Float -> (values Float Float Float))
                  (Float Float Float Float Float Float -> (values Float Float Float))))
  (define fl3-
    (case-lambda
      [(x y z)  (values (- x) (- y) (- z))]
      [(x1 y1 z1 x2 y2 z2)  (values (- x1 x2) (- y1 y2) (- z1 z2))]))
  
  (: fl3mag^2 (Float Float Float -> Float))
  (define (fl3mag^2 x y z)
    (+ (* x x) (* y y) (* z z)))
  
  (: fl3mag (Float Float Float -> Float))
  (define (fl3mag x y z)
    (flsqrt (fl3mag^2 x y z)))
  
  (: fl3dist (Float Float Float Float Float Float -> Float))
  (define (fl3dist x1 y1 z1 x2 y2 z2)
    (fl3mag (- x1 x2) (- y1 y2) (- z1 z2)))
  
  (: fl3normalize (Float Float Float -> (values Float Float Float)))
  (define (fl3normalize x y z)
    (define d (fl3mag x y z))
    (values (/ x d) (/ y d) (/ z d)))
  
  (: fl3-half-norm (Float Float Float Float Float Float -> (values Float Float Float)))
  (define (fl3-half-norm x1 y1 z1 x2 y2 z2)
    (fl3normalize (+ x1 x2) (+ y1 y2) (+ z1 z2)))
  
  ) ; begin-encourage-inline
