#lang racket/base

(require racket/flonum)

(provide flsplit
         fast-mono-fl+/error fast-fl+/error fl+/error
         fast-mono-fl-/error fast-fl-/error fl-/error
         fast-fl*/error fl*/error)

;(: flsplit (Flonum -> (Values Flonum Flonum)))
;; Splits a flonum into a two flonums `hi' and `lo' with 26 bits precision each, such that
;; |hi| >= |lo| and hi + lo = a. (The extra sign bit accounts for the missing bit.)
(define-syntax-rule (flsplit a-expr)
  (let ([a a-expr])
    (let* ([c  (fl* 134217729.0 a)]  ; 134217729 = (expt 2 (ceiling (/ 53 2)))
           [a-hi  (fl- c (fl- c a))]
           [a-lo  (fl- a a-hi)])
      (values a-hi a-lo))))

;(: fast-mono-fl+/error (Flonum Flonum -> (Values Flonum Flonum)))
;; Returns a+b and its rounding error
;; Assumes |a| >= |b|
(define-syntax-rule (fast-mono-fl+/error a-expr b-expr)
  (let ([a a-expr] [b b-expr])
    (let ([x  (+ a b)])
      (values x (- b (- x a))))))

;(: fast-mono-fl-/error (Flonum Flonum -> (Values Flonum Flonum)))
;; Returns a+b and its rounding error
;; Assumes |a| >= |b|
(define-syntax-rule (fast-mono-fl-/error a-expr b-expr)
  (let ([a a-expr] [b b-expr])
    (let ([x  (- a b)])
      (values x (- (- a x) b)))))

;(: fast-fl+/error (Flonum Flonum -> (Values Flonum Flonum)))
;; Returns a+b and its rounding error
(define-syntax-rule (fast-fl+/error a-expr b-expr)
  (let ([a a-expr] [b b-expr])
    (let* ([x  (fl+ a b)]
           [v  (fl- x a)])
      (values x (fl+ (fl- a (fl- x v)) (fl- b v))))))

;(: fast-fl-/error (Flonum Flonum -> (Values Flonum Flonum)))
;; Returns a-b and its rounding error
(define-syntax-rule (fast-fl-/error a-expr b-expr)
  (let ([a a-expr] [b b-expr])
    (let* ([x  (fl- a b)]
           [v  (fl- x a)])
      (values x (fl- (fl- a (fl- x v)) (fl+ b v))))))

;(: fast-fl*/error (Flonum Flonum -> (Values Flonum Flonum)))
;; Returns a*b and its rounding error
(define-syntax-rule (fast-fl*/error a-expr b-expr)
  (let ([a a-expr] [b b-expr])
    (let*-values ([(x)  (fl* a b)]
                  [(a-hi a-lo)  (flsplit a)]
                  [(b-hi b-lo)  (flsplit b)])
      (values x (fl- (fl* a-lo b-lo)
                     (fl- (fl- (fl- x (fl* a-hi b-hi))
                               (fl* a-lo b-hi))
                          (fl* a-hi b-lo)))))))

#;; If we had a fused multiply-add:
(define (fast-fl*/error a b)
  (let ([p  (* a b)])
    (values p (flfma a b (- p)))))

(define-syntax-rule (define-slow-wrapper name fast-flop/error op)
  (define-syntax-rule (name a-expr b-expr)
    (let ([a a-expr] [b b-expr])
      (let-values ([(x e)  (fast-flop/error a b)])
        (if (and (x . > . -inf.0) (x . < . +inf.0)
                 (e . > . -inf.0) (e . < . +inf.0))
            (values x e)
            (let* ([v  (op (inexact->exact a) (inexact->exact b))]
                   [x  (real->double-flonum v)])
              (if (and (x . > . -inf.0) (x . < . +inf.0))
                  (values x (real->double-flonum (- v (inexact->exact x))))
                  (values x 0.0))))))))

(define-slow-wrapper fl+/error fast-fl+/error +)
(define-slow-wrapper fl-/error fast-fl-/error -)
(define-slow-wrapper fl*/error fast-fl*/error *)
