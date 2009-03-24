;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; math.ss: some extra math routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang scheme/base
(provide pi
         sqr
         sgn conjugate
         sinh cosh tanh)

(define (sqr z) (* z z))

(define pi (atan 0 -1))

;; sgn function
(define (sgn x)
  (if (exact? x)
    (cond [(< x 0) -1] [(> x 0) 1] [else 0])
    (cond [(< x 0.0) -1.0] [(> x 0.0) 1.0] [else 0.0])))

;; complex conjugate
(define (conjugate z)
  (make-rectangular (real-part z) (- (imag-part z))))

;; real hyperbolic functions
(define (sinh x)
  (/ (- (exp x) (exp (- x))) 2.0))

(define (cosh x)
  (/ (+ (exp x) (exp (- x))) 2.0))

(define (tanh x) (/ (sinh x) (cosh x)))
