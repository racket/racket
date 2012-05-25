#lang typed/racket/base

(require (for-syntax typed/racket/base)
         (rename-in racket/flonum
                    [flvector-ref old:flvector-ref]
                    [flvector-set! old:flvector-set!])
         (except-in racket/fixnum fl->fx fx->fl)  ; these two functions are untyped
         racket/math
         (only-in racket/unsafe/ops unsafe-flvector-set! unsafe-fx+)
         racket/performance-hint)

(provide (all-defined-out)
         (except-out (all-from-out racket/flonum
                                   racket/fixnum)
                     old:flvector-ref
                     old:flvector-set!))

(define-predicate nonnegative-fixnum? Nonnegative-Fixnum)

;; This looks stupid, but it avoids an optimization TR does that is actually a pessimization, by
;; keeping it from recognizing flvector-ref
(: flvector-ref (FlVector Integer -> Flonum))
(define flvector-ref old:flvector-ref)

;; Ditto above
(: flvector-set! (FlVector Integer Flonum -> Void))
(define flvector-set! old:flvector-set!)

(define-syntax-rule (inline-build-flvector size f)
  (let: ([n : Integer  size])
    (with-asserts ([n  nonnegative-fixnum?])
      (define vs (make-flvector n))
      (let: loop : FlVector ([i : Nonnegative-Fixnum  0])
        (cond [(i . fx< . n)  (unsafe-flvector-set! vs i (f i))
                              (loop (unsafe-fx+ i 1))]
              [else  vs])))))

(begin-encourage-inline
  
  (: fx->fl (Fixnum -> Flonum))
  (define fx->fl ->fl)
  
  (: fl->fx (Flonum -> Fixnum))
  (define (fl->fx x)
    (define i (fl->exact-integer x))
    (with-asserts ([i fixnum?]) i))
  
  (: flrational? (Flonum -> Boolean))
  (define (flrational? x)
    ;; if x = +nan.0, both tests return #f
    (and (x . > . -inf.0) (x . < . +inf.0)))
  
  (: fl-convex-combination (Flonum Flonum Flonum -> Flonum))
  (define (fl-convex-combination dv sv sa)
    (+ (* sv sa) (* dv (- 1.0 sa))))
  
  (: fl-alpha-blend (Flonum Flonum Flonum -> Flonum))
  (define (fl-alpha-blend dca sca sa)
    (+ sca (* dca (- 1.0 sa))))
  
  (: flgaussian (Flonum Flonum -> Flonum))
  (define (flgaussian x s)
    (define x/s (/ x s))
    (/ (exp (* -0.5 (* x/s x/s)))
       (* (sqrt (* 2.0 pi)) s)))
  
  (: flsigmoid (Flonum -> Flonum))
  (define (flsigmoid x)
    (/ 1.0 (+ 1.0 (exp (- x)))))
  
  ;; =================================================================================================
  ;; 3-vectors
  
  (: fl3dot (Flonum Flonum Flonum Flonum Flonum Flonum -> Flonum))
  (define (fl3dot x1 y1 z1 x2 y2 z2)
    (+ (* x1 x2) (* y1 y2) (* z1 z2)))
  
  (: fl3* (case-> (Flonum Flonum Flonum Flonum -> (values Flonum Flonum Flonum))
                  (Flonum Flonum Flonum Flonum Flonum Flonum -> (values Flonum Flonum Flonum))))
  (define fl3*
    (case-lambda
      [(x y z c)  (values (* x c) (* y c) (* z c))]
      [(x1 y1 z1 x2 y2 z2)  (values (* x1 x2) (* y1 y2) (* z1 z2))]))
  
  (: fl3+ (Flonum Flonum Flonum Flonum Flonum Flonum -> (values Flonum Flonum Flonum)))
  (define (fl3+ x1 y1 z1 x2 y2 z2)
    (values (+ x1 x2) (+ y1 y2) (+ z1 z2)))
  
  (: fl3- (case-> (Flonum Flonum Flonum -> (values Flonum Flonum Flonum))
                  (Flonum Flonum Flonum Flonum Flonum Flonum -> (values Flonum Flonum Flonum))))
  (define fl3-
    (case-lambda
      [(x y z)  (values (- x) (- y) (- z))]
      [(x1 y1 z1 x2 y2 z2)  (values (- x1 x2) (- y1 y2) (- z1 z2))]))
  
  (: fl3mag^2 (Flonum Flonum Flonum -> Flonum))
  (define (fl3mag^2 x y z)
    (+ (* x x) (* y y) (* z z)))
  
  (: fl3mag (Flonum Flonum Flonum -> Flonum))
  (define (fl3mag x y z)
    (flsqrt (fl3mag^2 x y z)))
  
  (: fl3dist (Flonum Flonum Flonum Flonum Flonum Flonum -> Flonum))
  (define (fl3dist x1 y1 z1 x2 y2 z2)
    (fl3mag (- x1 x2) (- y1 y2) (- z1 z2)))
  
  (: fl3normalize (Flonum Flonum Flonum -> (values Flonum Flonum Flonum)))
  (define (fl3normalize x y z)
    (define d (fl3mag x y z))
    (values (/ x d) (/ y d) (/ z d)))
  
  (: fl3-half-norm (Flonum Flonum Flonum Flonum Flonum Flonum -> (values Flonum Flonum Flonum)))
  (define (fl3-half-norm x1 y1 z1 x2 y2 z2)
    (fl3normalize (+ x1 x2) (+ y1 y2) (+ z1 z2)))
  
  ) ; begin-encourage-inline
