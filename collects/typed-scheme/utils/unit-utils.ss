#lang scheme/base

(require scheme/unit (for-syntax scheme/base))

(provide cnt)

(define-signature-form (cnt stx)
  (syntax-case stx ()
    [(_ nm cnt)
     (list #'nm)
     #;(list #'[contracted (nm cnt)])]))



