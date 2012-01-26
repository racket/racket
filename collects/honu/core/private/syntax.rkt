#lang racket

(provide (all-defined-out))

(require (for-syntax syntax/define
                     "transformer.rkt"))

#|
(define honu-scheme-syntax 'honu-scheme-syntax)

(define (raw-scheme? stx)
  (syntax-property stx honu-scheme-syntax))

(define (apply-scheme-syntax stx)
  (syntax-property stx honu-scheme-syntax #t))
|#

(provide define-honu-syntax)
(define-syntax (define-honu-syntax stx)
  (let-values ([(id rhs) (normalize-definition stx #'lambda #f)])
    (with-syntax ([id id]
                  [rhs rhs])
      (syntax/loc stx
                 (define-syntax id (make-honu-transformer rhs))))))
