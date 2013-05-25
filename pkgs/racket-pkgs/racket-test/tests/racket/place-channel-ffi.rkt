#lang racket/base

(require racket/place
         ffi/unsafe
         openssl/libcrypto
         racket/runtime-path
         rackunit
         (for-syntax racket/base))

(define-syntax-rule (define-crypto-func name func-signature)
  (define name (and libcrypto (get-ffi-obj (quote name) libcrypto func-signature (lambda () #f)))))

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
                   (check-equal? (BN-j1 b) 1334)
                   
                   (place-channel-put ch (place-channel-get ch))))
  (place-channel-put p bn)

  (define-cstruct _S ([a _int]))
  (define s (malloc _S 'raw))
  (check-equal? #t (place-message-allowed? s))
  (place-channel-put p s)
  (check-equal? s (place-channel-get p))

  (place-wait p))
