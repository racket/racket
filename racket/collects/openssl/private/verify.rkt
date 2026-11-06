#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         "../libssl.rkt"
         "../libcrypto.rkt"
         "ffi.rkt")
(provide X509_V_OK
         (protect-out (all-defined-out)))

(define-logger openssl-verify #:parent openssl-logger)

;; ----------------------------------------

(define-crypto CRYPTO_free
  (_fun _pointer [_pointer = #f] [_int = 0] -> _void))

(define-crypto OPENSSL_sk_value
  (_fun _STACK* _int -> _pointer))

(define-cpointer-type _X509_STORE*)
(define-cpointer-type _X509_STORE_CTX*)

(define-crypto X509_STORE_CTX_get0_untrusted
  (_fun [ctx : #;const _X509_STORE_CTX*]
        -> _STACK* #;X509*))

(define-crypto X509_STORE_CTX_get0_cert
  (_fun [ctx : #;const _X509_STORE_CTX*]
        -> _X509*))

(define-crypto X509_VERIFY_PARAM_get0_host
  (_fun _X509_VERIFY_PARAM* _int -> _string/latin-1))

(define-crypto X509_STORE_CTX_get0_param
  (_fun _X509_STORE_CTX* -> _X509_VERIFY_PARAM*))

(define-crypto X509_STORE_CTX_set_error
  (_fun _X509_STORE_CTX* _int -> _void))

(define-crypto X509_STORE_CTX_set_error_depth
  (_fun _X509_STORE_CTX* _int -> _void))

(define-crypto i2d_X509
  (_fun [x : _X509*]
        [der : (_ptr io _pointer) = #f]
        -> [s : _int]
        -> (cond [(> s 0)
                  (define buf (make-bytes s))
                  (memcpy buf 0 der 0 s)
                  (CRYPTO_free der)
                  buf]
                 [else #f])))

;;(define X509_V_OK                                       0)
(define X509_V_ERR_UNSPECIFIED                          1)

;; ----------------------------------------

(define _cert-verify-callback
  (_fun [ctx : _X509_STORE_CTX*]
        [userdata : _pointer]
        -> _int))

(define-ssl SSL_CTX_set_cert_verify_callback
  (_fun [ctx : _SSL_CTX*]
        [callback : _fpointer]
        [userdata : _pointer]
        -> _void))

;; ============================================================

(define (store-extract store)
  (define cert (i2d_X509 (X509_STORE_CTX_get0_cert store)))
  (define untrusted-st (X509_STORE_CTX_get0_untrusted store))
  (define untrusted-certs
    (for/list ([i (in-range (sk_num untrusted-st))])
      (define x509 (cast (OPENSSL_sk_value untrusted-st i) _pointer _X509*))
      ;; (log-error "~s ~s\n" i x509)
      (i2d_X509 x509)))
  (define vparam (X509_STORE_CTX_get0_param store))
  (define hostname (X509_VERIFY_PARAM_get0_host vparam 0)) ;; get all?
  (values hostname cert untrusted-certs))

;; Purpose = (U 'server-auth 'client-auth)

;; make-verify-callback : (Purpose String/#f Bytes (Listof Bytes))
;;                     -> (X509_STORE_CTX* Void* -> Int)
(define ((make-verify-callback verifier) store userdata)
  (define-values (hostname cert-der intermediate-ders) (store-extract store))
  (define ok? (verifier 'server-auth hostname cert-der intermediate-ders))
  (cond [ok? (X509_STORE_CTX_set_error store X509_V_OK)]
        [else (X509_STORE_CTX_set_error store X509_V_ERR_UNSPECIFIED)])
  (if ok? 1 0))
