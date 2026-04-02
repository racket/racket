#lang racket/base
(require ffi2
         rackunit
         "make-ffi2-lib.rkt")

(define-values (test-lib clean-ffi2-lib)
  (build-ffi2-lib))

(define-ffi2-definer define-test-procedure #:lib test-lib)

(define-ffi2-type intfloat_t (struct
                               [i int_t]
                               [f float_t]))

(define-ffi2-type to_double_t (int_t float_t . -> . double_t))

(define-test-procedure intfloat_sum (intfloat_t . -> . double_t))
(define-test-procedure intfloat_sum_content (intfloat_t* . -> . double_t))
(define-test-procedure intfloat_build (int_t float_t . -> . intfloat_t))
(define-test-procedure double_built (to_double_t . -> . double_t))
(define-test-procedure multiply_built (to_double_t int_t . -> . double_t))
(define-test-procedure multiply_built2 (to_double_t
                                        to_double_t
                                        int_t
                                        . -> . double_t))
(define-test-procedure intfloat_sum_built ((int_t float_t . -> . intfloat_t)
                                           . -> . double_t))

(check-equal? (intfloat_sum (intfloat_t 2 3.5)) 5.5)
(check-equal? (intfloat_sum_content (intfloat_t 2 3.5)) 5.5)

(let ()
  (define n2 (intfloat_build 5 6.5))
  (check-equal? (intfloat_t-i n2) 5)
  (check-equal? (intfloat_t-f n2) 6.5))

(check-equal? (double_built (lambda (i f) (+ i f)))
              220.0)
(check-equal? (intfloat_sum_built (lambda (i f) (intfloat_t i f)))
              110.0)

(check-equal? (multiply_built (lambda (i f) (+ i f (multiply_built (lambda (i f) (+ i f)) 3)))
                              10)
              4400.0)
(check-equal? (multiply_built2 (lambda (i f) (+ i f))
                               (lambda (i f) (collect-garbage) (- i f))
                               10)
              200.0)

(clean-ffi2-lib)
