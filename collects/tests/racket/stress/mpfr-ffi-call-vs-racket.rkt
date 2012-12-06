#lang racket/base

(require ffi/unsafe
         tests/stress
         math/private/bigfloat/mpfr)

(define mpfr-get-exp (get-mpfr-fun 'mpfr_get_exp (_fun _mpfr-pointer -> _exp_t)))
(define mpfr-get-prec (get-mpfr-fun 'mpfr_get_prec (_fun _mpfr-pointer -> _prec_t)))
(define mpfr-get-signbit (get-mpfr-fun 'mpfr_signbit (_fun _mpfr-pointer -> _int)))

(define (mpfr-signbit x)
  (if ((mpfr-sign x) . < . 0) 1 0))

(define n 1000000)

(let ([x  (bf 2)])
  (stress
   20
   ["mpfr-get-prec (FFI accessor)"
    (for ([_  (in-range n)])
      (mpfr-get-prec x))]
   ["mpfr-prec (Racket accessor)"
    (for ([_  (in-range n)])
      (mpfr-prec x))]))

(let ([x  (bf 2)])
  (stress
   20
   ["mpfr-get-exp (FFI accessor)"
    (for ([_  (in-range n)])
      (mpfr-get-exp x))]
   ["mpfr-exp (Racket accessor)"
    (for ([_  (in-range n)])
      (mpfr-exp x))]))

(let ([x  (bf 2)])
  (stress
   20
   ["mpfr-get-signbit (FFI accessor)"
    (for ([_  (in-range n)])
      (mpfr-get-signbit x))]
   ["mpfr-signbit (Racket accessor)"
    (for ([_  (in-range n)])
      (mpfr-signbit x))]))
