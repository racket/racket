#lang racket/base

(provide flsplit
         fast-mono-fl+/error
         fast-mono-fl-/error
         fast-fl+/error
         fast-fl-/error
         fast-fl*/error
         fast-flsqr/error
         fast-fl//error
         fast-flfma/error
         fl+/error
         fl-/error
         fl*/error
         flsqr/error
         fl//error
         flfma/error)

(module untyped-defs racket/base
  (require (for-syntax racket/base)
           "flonum-functions.rkt")
  
  (provide (all-defined-out))
  
  ;(: flsplit (Flonum -> (Values Flonum Flonum)))
  ;; Splits a flonum into a two flonums `hi' and `lo' with 26 bits precision each, such that
  ;; |hi| >= |lo| and hi + lo = a. (The extra sign bit accounts for the missing bit.)
  ;; This function returns (values +nan.0 +nan.0) for |a| >= 1.3393857490036326e+300.
  (define-syntax-rule (flsplit a-expr)
    (let ([a a-expr])
      (let* ([c   (fl* a (fl+ 1.0 (flexpt 2.0 27.0)))]
             [x2  (fl- c (fl- c a))])
        (values x2 (fl- a x2)))))
  
  ;; =================================================================================================
  ;; Fast monotone addition and subtraction
  
  ;(: fast-mono-fl+/error (Flonum Flonum -> (Values Flonum Flonum)))
  ;; Returns a+b and its rounding error
  ;; Assumes |a| >= |b|
  (define-syntax-rule (fast-mono-fl+/error a-expr b-expr)
    (let ([a a-expr] [b b-expr])
      (let ([x2  (+ a b)])
        (values x2 (- b (- x2 a))))))
  
  ;(: fast-mono-fl-/error (Flonum Flonum -> (Values Flonum Flonum)))
  ;; Returns a+b and its rounding error
  ;; Assumes |a| >= |b|
  (define-syntax-rule (fast-mono-fl-/error a-expr b-expr)
    (let ([a a-expr] [b b-expr])
      (let ([x2  (- a b)])
        (values x2 (- (- a x2) b)))))
  
  ;; =================================================================================================
  ;; Fast arithmetic that returns rounding error
  
  ;(: fast-fl+/error (Flonum Flonum -> (Values Flonum Flonum)))
  ;; Returns a+b and its rounding error
  (define-syntax-rule (fast-fl+/error a-expr b-expr)
    (let ([a a-expr] [b b-expr])
      (let* ([x2  (fl+ a b)]
             [v   (fl- x2 a)])
        (values x2 (fl+ (fl- a (fl- x2 v)) (fl- b v))))))
  
  ;(: fast-fl-/error (Flonum Flonum -> (Values Flonum Flonum)))
  ;; Returns a-b and its rounding error
  (define-syntax-rule (fast-fl-/error a-expr b-expr)
    (let ([a a-expr] [b b-expr])
      (let* ([x2  (fl- a b)]
             [v   (fl- x2 a)])
        (values x2 (fl- (fl- a (fl- x2 v)) (fl+ b v))))))
  
  ;(: fast-fl*/error (Flonum Flonum -> (Values Flonum Flonum)))
  ;; Returns a*b and its rounding error
  (define-syntax-rule (fast-fl*/error a-expr b-expr)
    (let ([a a-expr] [b b-expr])
      (let*-values ([(x2)  (fl* a b)]
                    [(a2 a1)  (flsplit a)]
                    [(b2 b1)  (flsplit b)])
        (values x2 (- (fl- (fl- (fl- (fl- x2 (fl* a2 b2))
                                     (fl* a1 b2))
                                (fl* a2 b1))
                           (fl* a1 b1)))))))
  
  ;(: fast-flfma/error (Flonum Flonum Flonum -> (Values Flonum Flonum)))
  ;; Returns a*b+c and its rounding error
  (define-syntax-rule (fast-flfma/error a-expr b-expr c-expr)
    (let*-values ([(y2 y1)  (fast-fl*/error a-expr b-expr)]
                  [(h0 h1)  (fast-fl+/error c-expr y1)]
                  [(h3 h2)  (fast-fl+/error h0 y2)])
      (values h3 (fl+ h2 h1))))
  
  #;; If we had hardware fused multiply-add:
  (define-syntax-rule (fast-fl*/error a-expr b-expr)
    (let ([a a-expr] [b b-expr])
      (let ([x2  (fl* a b)])
        (values x2 (flfma a b (- x2))))))

  ;(: fast-flsqr/error (Flonum -> (Values Flonum Flonum)))
  ;; Returns a*a and its rounding error
  (define-syntax-rule (fast-flsqr/error a-expr)
    (let ([a a-expr])
      (let*-values ([(x2)  (fl* a a)]
                    [(a2 a1)  (flsplit a)])
        (values x2 (- (fl- (fl- (fl- x2 (fl* a2 a2))
                                (fl* 2.0 (fl* a2 a1)))
                           (fl* a1 a1)))))))
  
  ;(: fast-fl//error (Flonum Flonum -> (Values Flonum Flonum)))
  ;; Returns a/b and its rounding error
  (define-syntax-rule (fast-fl//error a-expr b-expr)
    (let ([a a-expr] [b b-expr])
      (let*-values ([(x2)  (fl/ a b)]
                    [(w2 w1)  (fast-fl*/error x2 b)])
        (fast-mono-fl+/error x2 (fl/ (fl- (fl- a w2) w1) b)))))
  
  )  ; module

(require (submod "." untyped-defs))

(module typed-defs typed/racket/base
  (require racket/performance-hint
           (submod ".." untyped-defs)
           "flonum-functions.rkt"
           "utils.rkt")
  
  (provide (all-defined-out))
  
  ;; =================================================================================================
  ;; Function versions of the above that are well-defined for the largest domain, and return 0.0 as
  ;; the second argument whenever the first isn't rational
  
  (begin-encourage-inline
    
    (: fl+/error (Flonum Flonum -> (Values Flonum Flonum)))
    (define (fl+/error a b)
      (let-values ([(x2 x1)  (fast-fl+/error a b)])
        (values x2 (if (flrational? x2) x1 0.0))))
    
    (: fl-/error (Flonum Flonum -> (Values Flonum Flonum)))
    (define (fl-/error a b)
      (let-values ([(x2 x1)  (fast-fl-/error a b)])
        (values x2 (if (flrational? x2) x1 0.0))))
    
    (: fl*/error (Flonum Flonum -> (Values Flonum Flonum)))
    (define (fl*/error a b)
      (let ([x2  (fl* a b)])
        (values x2 (if (and (flrational? x2) (not (flsubnormal? x2)))
                       (let*-values ([(da db)  (values (near-pow2 a) (near-pow2 b))]
                                     [(d)  (fl* da db)]
                                     [(d?)  (and (d . fl> . 0.0) (d . fl< . +inf.0))]
                                     [(a2 a1)  (flsplit (fl/ a da))]
                                     [(b2 b1)  (flsplit (fl/ b db))]
                                     [(x2)  (if d? (fl/ x2 d) (fl/ (fl/ x2 da) db))]
                                     [(x1)  (- (fl- (fl- (fl- (fl- x2 (fl* a2 b2))
                                                              (fl* a1 b2))
                                                         (fl* a2 b1))
                                                    (fl* a1 b1)))])
                         (if d? (fl* x1 d) (fl* (fl* x1 da) db)))
                       0.0))))
    
    (: flsqr/error (Flonum -> (Values Flonum Flonum)))
    (define (flsqr/error a)
      (let ([x2  (fl* a a)])
        (values x2 (if (and (flrational? x2) (not (flsubnormal? x2)))
                       (let*-values ([(d)  (near-pow2 a)]
                                     [(d^2)  (fl* d d)]
                                     [(d^2?)  (and (d^2 . fl> . 0.0) (d^2 . fl< . +inf.0))]
                                     [(a2 a1)  (flsplit (fl/ a d))]
                                     [(x2)  (if d^2? (fl/ x2 d^2) (fl/ (fl/ x2 d) d))]
                                     [(x1)  (- (fl- (fl- (fl- x2 (fl* a2 a2))
                                                         (fl* 2.0 (fl* a1 a2)))
                                                    (fl* a1 a1)))])
                         (if d^2? (fl* x1 d^2) (fl* (fl* x1 d) d)))
                       0.0))))
    
    (: fl//error (Flonum Flonum -> (Values Flonum Flonum)))
    (define (fl//error a b)
      (let ([x2  (fl/ a b)])
        (values x2 (if (and (flrational? x2) (flrational? b))
                       (let* ([d  (near-pow2/div a b)]
                              [a  (fl/ a d)]
                              [b  (fl/ b d)])
                         (let-values ([(w2 w1)  (fl*/error x2 b)])
                           (fl/ (fl- (fl- a w2) w1) b)))
                       0.0))))
    
    (: flfma/error (-> Flonum Flonum Flonum (Values Flonum Flonum)))
    (define (flfma/error a b c)
      (define-values (x2 x1) (fast-flfma/error a b c))
      (cond [(flrational? (+ x2 x1))  (values x2 x1)]
            [else
             (define n (near-pow2 (max (flsqrt (abs a)) (flsqrt (abs b)))))
             (define 1/n (/ 1.0 n))
             (define n^2 (* n n))
             (let-values ([(x2 x1)  (fast-flfma/error (* a 1/n) (* b 1/n) (* c 1/n 1/n))])
               (values (* n^2 x2) (* n^2 x1)))]))
    
    )  ; begin-encourage-inline
  
  )  ; module

(require (submod "." typed-defs))
