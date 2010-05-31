#lang scheme/base
(require scheme/foreign)
(unsafe!)

(provide define-mz)

(define-syntax-rule (define-mz id type)
  (define id (get-ffi-obj 'id #f type)))
