#lang racket/base

(require (for-syntax racket/base))

(provide syntax-value)

(define-syntax (syntax-value stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax/loc stx (syntax-value expr (λ (x) x) (λ (x) x)))]
    [(_ expr encode decode)
     (syntax/loc stx
       (let ()
         (define-syntax (make-constant inner-stx)
           (define val expr)
           (with-syntax ([enc-val  (datum->syntax inner-stx (encode val))])
             (syntax/loc inner-stx (decode enc-val))))
         (make-constant)))]))
