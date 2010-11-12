#lang scheme

(require (for-syntax syntax/parse))

(provide (all-defined-out))

(define-syntax (field-access stx)
  (syntax-parse stx
    [(_ field object)
     #'(get-field field object)]))
