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
     (case (if runtime? (system-type 'arch) (cross-system-type 'arch))
       [(i386 ppc)
        ;; Version "1.1" is bundled with Racket
        '(so "libssl" ("1.1" #f))]
       [else
        ;; Version "3" is bundled with Racket
        '(so "libssl" ("3" #f))])]
    [else '(so "libssl")]))

(define libssl
  (and libcrypto
       (if (eq? (system-type 'os*) 'ios)
           ;; If libcrypto has been loaded and we're on iOS, assume
           ;; that libssl has also been loaded. See the comment in
           ;; libcrypto.rkt for details.
           (ffi-lib #f)
           (with-handlers ([exn:fail?
                            (lambda (x)
                              (set! libssl-load-fail-reason (exn-message x))
                              #f)])
             (ffi-lib libssl-so openssl-lib-versions)))))
