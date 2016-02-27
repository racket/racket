#lang racket/base
(require ffi/unsafe
         racket/runtime-path
         setup/cross-system
         (for-syntax racket/base
                     setup/cross-system)
         "libcrypto.rkt")

(provide libssl
         libssl-load-fail-reason)

(define libssl-load-fail-reason #f)

;; We need to declare because they might be distributed with PLT Scheme
;; in which case they should get bundled with stand-alone executables:
(define-runtime-path libssl-so
  #:runtime?-id runtime?
  (case (if runtime? (system-type) (cross-system-type))
    [(windows) '(so "ssleay32")]
    [(macosx)
     ;; Version "1.0.0" is bundled with Racket
     '(so "libssl" ("1.0.0" #f))]
    [else '(so "libssl")]))

(define libssl
  (and libcrypto
       (with-handlers ([exn:fail?
                        (lambda (x)
                          (set! libssl-load-fail-reason (exn-message x))
                          #f)])
         (ffi-lib libssl-so openssl-lib-versions))))
