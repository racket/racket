#lang scheme/base

(require syntax/stx
         syntax/kerncase
         syntax/struct
         scheme/include)

(provide shared/proc)

(require (for-template
          scheme/base
          (only-in "teachprims.ss" [advanced-cons the-cons])))

(define shared/proc
  (lambda (stx make-check-cdr undefined-expr)
    (with-syntax ([undefined undefined-expr])
      ;; Include the implementation.
      ;; See private/shared-body.ss.
      (include (lib "mzlib/private/shared-body.ss")))))
