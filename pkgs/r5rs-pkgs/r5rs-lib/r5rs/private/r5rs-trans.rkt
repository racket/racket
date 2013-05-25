#lang scheme/base
(require (for-syntax scheme/base))
(provide syntax-rules-only)

(define-syntax (syntax-rules-only stx)
  (syntax-case stx ()
    [(_ . form)
     (raise-syntax-error
      'macro-transformer
      "only a `syntax-rules' form is allowed"
      #'form)]))
