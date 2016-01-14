#lang racket/base

(provide ensure-writable
         done-writable)

;; Returns #f (no change needed) or old permissions
(define (ensure-writable dest-exe)
  (cond
   [(member 'write (file-or-directory-permissions dest-exe))
    ;; No change needed
    #f]
   [else
    (define old-perms
      (file-or-directory-permissions dest-exe 'bits))
    (file-or-directory-permissions dest-exe (bitwise-ior old-perms #o200))
    old-perms]))

;; Restores old permissions (if not #f)
(define (done-writable dest-exe old-perms)
  (when old-perms
    (file-or-directory-permissions dest-exe old-perms)))
