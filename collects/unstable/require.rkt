#lang racket/base
(require (for-syntax racket/base
                     racket/require-transform
                     racket/provide-transform
                     syntax/parse))
(provide require/provide
         quote-require)

(define-syntax-rule (require/provide spec ...)
  (begin
    (define-syntax imports (box #f))
    (require (box-require imports (combine-in spec ...)))
    (provide (box-provide imports))))

(define-syntax (quote-require stx)
  (syntax-parse stx
    [(_ spec:expr ...)
     (let*-values ([(imports sources)
                    (expand-import (syntax/loc stx (combine-in spec ...)))])
       (with-syntax ([(name ...) (map import-local-id imports)])
         (syntax/loc stx '(name ...))))]))

(define-syntax box-require
  (make-require-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ ibox spec:expr)
        #:declare ibox (static box? "mutable box for expanded import specs")
        (let-values ([(imports sources) (expand-import #'spec)])
          (set-box! (syntax-local-value #'ibox) imports)
          (values imports sources))]))))

(define-for-syntax (import->export i)
  (make-export (import-local-id i)
               (syntax-e (import-local-id i))
               (import-mode i)
               #f
               (import-orig-stx i)))

(define-syntax box-provide
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ ibox)
        #:declare ibox (static box? "mutable box for expanded import specs")
        (map import->export (unbox (syntax-local-value #'ibox)))]))))
