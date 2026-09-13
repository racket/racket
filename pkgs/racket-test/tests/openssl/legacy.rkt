#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         openssl/libcrypto
         openssl/legacy)

(define EVP_CIPHER_fetch (get-ffi-obj 'EVP_CIPHER_fetch
                                      libcrypto
                                      (_fun _pointer _string _pointer -> _pointer)
                                      ;; if not available, maybe it's not OpenSSL 3
                                      (lambda () (lambda (ctx name option) 'whatever))))

(unless (EVP_CIPHER_fetch #f "RC4" #f)
  (error "could not get legacy cipher RC4"))
