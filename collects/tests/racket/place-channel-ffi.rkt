#lang racket/base

(require racket/place
         ffi/unsafe
         racket/runtime-path
         rackunit
         (for-syntax racket/base))

(define-runtime-path libcrypto-so
  (case (system-type)
    [(windows) '(so "libeay32")]
    [else '(so "libcrypto")]))

(define libcrypto
  (with-handlers ([exn:fail? (lambda (exn) 
                               (log-warning (format "warning: couldn't load OpenSSL library: ~a"
                                                    (if (exn? exn)
                                                        (exn-message exn)
                                                        exn)))
                               #f)])
    (ffi-lib libcrypto-so '("" "0.9.8b" "0.9.8" "0.9.7"))))

(define-syntax-rule (define-crypto-func name func-signature)
  (begin
    (define name (and libcrypto (get-ffi-obj (quote name) libcrypto func-signature (lambda () #f))))
    (provide name)))


(define-cstruct _BN ([j1 _long] [top _int] [dmax _int] [neg _int] [flags _int]))
(define-crypto-func BN_new (_fun -> _BN-pointer))

(module+ test
  (main))

(define (main)
  (define bn (BN_new))
  (set-BN-j1! bn 1334)
  (printf "BN-j1 ~a ~v\n" (BN-j1 bn) (cpointer-tag bn))
  (check-equal? (BN-j1 bn) 1334)
  (check-equal? (cpointer-tag bn) 'BN)
  (check-equal? BN-tag 'BN)
  (printf "BN tag ~v\n" BN-tag)
  (define p (place ch
                   (define b (place-channel-get ch))
                   (printf "Got it ~a\n" (BN-j1 b))
                   (check-equal? (BN-j1 b) 1334)))
  (place-channel-put p bn)
  (place-wait p))
