#lang scheme/base

(require (for-syntax scheme/base))

(provide define-provide/doc-transformer
         (for-syntax
          provide/doc-transformer?
          provide/doc-transformer-proc))

(begin-for-syntax
 (define-struct provide/doc-transformer (proc) #:omit-define-syntaxes))

(define-syntax-rule (define-provide/doc-transformer id rhs)
  (define-syntax id
    (make-provide/doc-transformer rhs)))
