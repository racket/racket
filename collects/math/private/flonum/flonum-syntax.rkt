#lang racket/base

(require (for-syntax racket/base)
         racket/flonum)

(provide flsplit
         fast-mono-fl+/error fast-fl+/error fl+/error
         fast-mono-fl-/error fast-fl-/error fl-/error
         fast-fl*/error fast-flsqr/error fl*/error
         fast-fl//error)

;(: flsplit (Flonum -> (Values Flonum Flonum)))
;; Splits a flonum into a two flonums `hi' and `lo' with 26 bits precision each, such that
;; |hi| >= |lo| and hi + lo = a. (The extra sign bit accounts for the missing bit.)
(define-syntax-rule (flsplit a-expr)
  (let ([a a-expr])
    (let* ([s  (if ((flabs a) . fl< . 1e300) 1.0 (flexpt 2.0 52.0))]
           [a  (fl/ a s)]
           [c  (fl* a (fl+ 1.0 (flexpt 2.0 27.0)))]
           [a-hi  (fl- c (fl- c a))]
           [a-lo  (fl- a a-hi)])
      (values (fl* a-hi s)
              (fl* a-lo s)))))

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
      (values x (- (fl- (fl- (fl- (fl- x (fl* a-hi b-hi))
                                  (fl* a-lo b-hi))
                             (fl* a-hi b-lo))
                        (fl* a-lo b-lo)))))))

(define-syntax-rule (fast-flsqr/error a-expr)
  (let ([a a-expr])
    (let*-values ([(x)  (fl* a a)]
                  [(a-hi a-lo)  (flsplit a)])
      (values x (- (fl- (fl- (fl- x (fl* a-hi a-hi))
                             (fl* 2.0 (fl* a-hi a-lo)))
                        (fl* a-lo a-lo)))))))

;(: fast-fl//error (Flonum Flonum -> (Values Flonum Flonum)))
;; Returns a/b and its rounding error
(define-syntax-rule (fast-fl//error a-expr b-expr)
  (let ([a a-expr] [b b-expr])
    (let*-values ([(q-hi)  (fl/ a b)]
                  [(q0 q1)  (fast-fl*/error q-hi b)])
      (values q-hi (fl/ (fl+ (- q1) (fl- a q0)) b)))))

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
