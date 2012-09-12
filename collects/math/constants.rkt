#lang typed/racket/base

(provide pi.0 pi.f
         phi.0 phi.f
         euler.0 euler.f
         gamma.0 gamma.f
         catalan.0 catalan.f)

(define pi.0 (real->double-flonum #e3.141592653589793238462643383279502884195))
(define pi.f (real->single-flonum #e3.141592653589793238462643383279502884195))

(define phi.0 (real->double-flonum #e1.61803398874989484820458683436563811772))
(define phi.f (real->single-flonum #e1.61803398874989484820458683436563811772))

(define euler.0 (real->double-flonum #e2.718281828459045235360287471352662497759))
(define euler.f (real->single-flonum #e2.718281828459045235360287471352662497759))

(define gamma.0 (real->double-flonum #e0.5772156649015328606065120900824024310432))
(define gamma.f (real->single-flonum #e0.5772156649015328606065120900824024310432))

(define catalan.0 (real->double-flonum #e0.9159655941772190150546035149323841107734))
(define catalan.f (real->single-flonum #e0.9159655941772190150546035149323841107734))
