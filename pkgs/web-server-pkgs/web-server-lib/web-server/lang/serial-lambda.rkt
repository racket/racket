#lang racket/base
(require racket/contract
         racket/list
         racket/serialize
         (for-syntax racket/base
                     web-server/lang/closure
                     web-server/lang/labels))

(define-syntax (serial-lambda stx)
  (syntax-case stx ()
    [(_ . lmbda-stx)
     (let ([labeling (make-labeling (string->bytes/utf-8 (format "~a" (syntax->datum stx))))])
       (make-closure
        (quasisyntax/loc stx
          (_ #,(labeling) (lambda . lmbda-stx)))))]))

(define-syntax (serial-case-lambda stx)
  (syntax-case stx ()
    [(_ . lmbda-stx)
     (let ([labeling (make-labeling (string->bytes/utf-8 (format "~a" (syntax->datum stx))))])
       (make-closure
        (quasisyntax/loc stx
          (_ #,(labeling) (case-lambda . lmbda-stx)))))]))

(provide serial-lambda
         serial-case-lambda)

(provide/contract
 [closure->deserialize-name (serializable? . -> . symbol?)])
(define (closure->deserialize-name proc)
  (string->symbol (cdr (first (third (serialize proc))))))
