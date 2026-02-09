#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         "../libssl.rkt"
         "../libcrypto.rkt")
(provide (protect-out
          (except-out (all-defined-out)
                      define-crypto
                      define-ssl)))

;; -- libcrypto

(define-ffi-definer define-crypto libcrypto
  #:default-make-fail make-not-available)
(define-cpointer-type _X509*)
(define-cpointer-type _X509_STORE*)

;; FIXME: refcounting?
(define-crypto d2i_X509
  (_fun (buf) ::
        (_pointer = #f)
        ((_ptr i _pointer) = buf)
        (_int32 = (bytes-length buf))
        -> _X509*))

(define-crypto X509_STORE_add_cert
  (_fun _X509_STORE* _X509* -> _int))

;; -- libssl

(define-ffi-definer define-ssl libssl
  #:default-make-fail make-not-available)
(define _SSL_CTX* _pointer)

(define-ssl SSL_CTX_get_cert_store
  (_fun _SSL_CTX* -> _X509_STORE*))
