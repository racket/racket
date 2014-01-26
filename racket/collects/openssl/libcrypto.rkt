#lang racket/base
(require ffi/unsafe
         racket/runtime-path
         (for-syntax racket/base))

(provide libcrypto
         libcrypto-load-fail-reason)

(define libcrypto-load-fail-reason #f)

;; We need to declare because they might be distributed with Racket,
;; in which case they should get bundled with stand-alone executables:
(define-runtime-path libcrypto-so
  (case (system-type)
    [(windows) '(so "libeay32")]
    [else '(so "libcrypto")]))

(define libcrypto
  (with-handlers ([exn:fail? (lambda (x)
                               (set! libcrypto-load-fail-reason (exn-message x))
                               #f)])
    (ffi-lib libcrypto-so '(""
                            "1.0.1e"
                            "1.0.0" "1.0"
                            "0.9.8b" "0.9.8" "0.9.7"))))
