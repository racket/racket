#lang scheme/base

(require (for-syntax scheme/base)
         (for-template scheme/base))

(provide inline-rules)

(define-syntax-rule (inline-rules orig-id [pat result] ...)
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [(set! . _)
        (raise-syntax-error #f
                            "cannot mutate"
                            stx)]
       [pat #'result] ...
       [(id . args) #'(orig-id . args)]
       [id #'orig-id]))))
