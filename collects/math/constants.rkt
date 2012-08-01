#lang typed/racket/base

(require (for-syntax racket/base "exact.rkt")
         "private/syntax.rkt")

(provide pi.0 pi.f
         phi.0 phi.f
         gamma.0 gamma.f
         catalan.0 catalan.f)

;; Dog-fooding the exact constants...
(begin-for-syntax
  
  (define-values (pi.0 phi.0)
    (parameterize ([exact-bits 53])
      (values (real->double-flonum (exact-pi))
              (real->double-flonum (exact-phi)))))
  
  (define-values (pi.f phi.f)
    (parameterize ([exact-bits 24])
      (values (real->single-flonum (exact-pi))
              (real->single-flonum (exact-phi)))))
  
  )  ; begin-for-syntax

(define pi.0 (syntax-value pi.0))
(define pi.f (syntax-value pi.f))

(define phi.0 (syntax-value phi.0))
(define phi.f (syntax-value phi.f))

(define gamma.0 (real->double-flonum #e0.5772156649015328606065120900824024310432))
(define gamma.f (real->single-flonum #e0.5772156649015328606065120900824024310432))

(define catalan.0 (real->double-flonum #e0.9159655941772190150546035149323841107734))
(define catalan.f (real->single-flonum #e0.9159655941772190150546035149323841107734))
