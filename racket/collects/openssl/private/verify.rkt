#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         "../libssl.rkt"
         "../libcrypto.rkt"
         "ffi.rkt")
(provide X509_V_OK
         (protect-out (all-defined-out)))

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
;; (define X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT            2)
;; (define X509_V_ERR_UNABLE_TO_GET_CRL                    3)
;; (define X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE     4)
;; (define X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE      5)
;; (define X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY   6)
(define X509_V_ERR_CERT_SIGNATURE_FAILURE               7)
;; (define X509_V_ERR_CRL_SIGNATURE_FAILURE                8)
(define X509_V_ERR_CERT_NOT_YET_VALID                   9)
(define X509_V_ERR_CERT_HAS_EXPIRED                     10)
;; (define X509_V_ERR_CRL_NOT_YET_VALID                    11)
;; (define X509_V_ERR_CRL_HAS_EXPIRED                      12)
;; (define X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD       13)
;; (define X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD        14)
;; (define X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD       15)
;; (define X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD       16)
;; (define X509_V_ERR_OUT_OF_MEM                           17)
;; (define X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT          18)
;; (define X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN            19)
;; (define X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY    20)
;; (define X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE      21)
(define X509_V_ERR_CERT_CHAIN_TOO_LONG                  22)
(define X509_V_ERR_CERT_REVOKED                         23)
;; (define X509_V_ERR_NO_ISSUER_PUBLIC_KEY                 24)
(define X509_V_ERR_PATH_LENGTH_EXCEEDED                 25)
(define X509_V_ERR_INVALID_PURPOSE                      26)
(define X509_V_ERR_CERT_UNTRUSTED                       27)
(define X509_V_ERR_CERT_REJECTED                        28)
;; (define X509_V_ERR_SUBJECT_ISSUER_MISMATCH              29)
;; (define X509_V_ERR_AKID_SKID_MISMATCH                   30)
;; (define X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH          31)
;; (define X509_V_ERR_KEYUSAGE_NO_CERTSIGN                 32)
;; (define X509_V_ERR_UNABLE_TO_GET_CRL_ISSUER             33)
(define X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION         34)
;; (define X509_V_ERR_KEYUSAGE_NO_CRL_SIGN                 35)
;; (define X509_V_ERR_UNHANDLED_CRITICAL_CRL_EXTENSION     36)
;; (define X509_V_ERR_INVALID_NON_CA                       37)
;; (define X509_V_ERR_PROXY_PATH_LENGTH_EXCEEDED           38)
;; (define X509_V_ERR_KEYUSAGE_NO_DIGITAL_SIGNATURE        39)
;; (define X509_V_ERR_PROXY_CERTIFICATES_NOT_ALLOWED       40)
(define X509_V_ERR_INVALID_EXTENSION                    41)
(define X509_V_ERR_INVALID_POLICY_EXTENSION             42)
(define X509_V_ERR_NO_EXPLICIT_POLICY                   43)
;; (define X509_V_ERR_DIFFERENT_CRL_SCOPE                  44)
;; (define X509_V_ERR_UNSUPPORTED_EXTENSION_FEATURE        45)
;; (define X509_V_ERR_UNNESTED_RESOURCE                    46)
(define X509_V_ERR_PERMITTED_VIOLATION                  47)
(define X509_V_ERR_EXCLUDED_VIOLATION                   48)
(define X509_V_ERR_SUBTREE_MINMAX                       49)
(define X509_V_ERR_APPLICATION_VERIFICATION             50)
(define X509_V_ERR_UNSUPPORTED_CONSTRAINT_TYPE          51)
(define X509_V_ERR_UNSUPPORTED_CONSTRAINT_SYNTAX        52)
(define X509_V_ERR_UNSUPPORTED_NAME_SYNTAX              53)
;; (define X509_V_ERR_CRL_PATH_VALIDATION_ERROR            54)
(define X509_V_ERR_PATH_LOOP                            55)
;; (define X509_V_ERR_SUITE_B_INVALID_VERSION              56)
;; (define X509_V_ERR_SUITE_B_INVALID_ALGORITHM            57)
;; (define X509_V_ERR_SUITE_B_INVALID_CURVE                58)
;; (define X509_V_ERR_SUITE_B_INVALID_SIGNATURE_ALGORITHM  59)
;; (define X509_V_ERR_SUITE_B_LOS_NOT_ALLOWED              60)
;; (define X509_V_ERR_SUITE_B_CANNOT_SIGN_P_384_WITH_P_256 61)
(define X509_V_ERR_HOSTNAME_MISMATCH                    62)
(define X509_V_ERR_EMAIL_MISMATCH                       63)
(define X509_V_ERR_IP_ADDRESS_MISMATCH                  64)

;; ----------------------------------------

(define-ssl SSL_CTX_set_cert_verify_callback
  (_fun [ctx : _SSL_CTX*]
        [callback : (_fun [ctx : _X509_STORE_CTX*]
                          [userdata : _pointer]
                          -> _int)]
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

;; make-verify-callback : (Purpose String/#f Bytes (Listof Bytes) (Nat Int -> Void))
;;                     -> (X509_STORE_CTX* Void* -> Int)
(define ((make-verify-callback verifier) store userdata)
  (define saved-error #f) ;; #f or (cons depth errorcode)
  (define (set-error depth code) (set! saved-error (cons depth code)))
  (define-values (hostname cert-der intermediate-ders) (store-extract store))
  (define ok? (verifier 'server-auth hostname cert-der intermediate-ders set-error))
  (cond [ok? (X509_STORE_CTX_set_error store X509_V_OK)]
        [(pair? saved-error)
         (X509_STORE_CTX_set_error_depth store (car saved-error))
         (X509_STORE_CTX_set_error store (cdr saved-error))]
        [else (X509_STORE_CTX_set_error store X509_V_ERR_UNSPECIFIED)])
  (if ok? 1 0))
