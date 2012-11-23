;; Support for loading root cerficates from Windows certificate store.

#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         "../libssl.rkt"
         "../libcrypto.rkt")
(provide load-win32-root-certificates)

;; -- libcrypto
(define-ffi-definer define-crypto libcrypto)
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

(define-ffi-definer define-ssl libssl)
(define _SSL_CTX* _pointer)

(define-ssl SSL_CTX_get_cert_store
  (_fun _SSL_CTX* -> _X509_STORE*))

;; -- Windows CryptoAPI

(define crypt-lib (ffi-lib "crypt32.dll"))
(define-ffi-definer define-crypt crypt-lib
  #:default-make-fail make-not-available)

(define _DWORD _int32)
(define-cpointer-type _CERTSTORE)
(define-cstruct _sCERT_CONTEXT
  ([certEncodingType _int32]
   [certEncoded _pointer]
   [certEncodedLen _int32]
   [certInfo _pointer]
   [certStore _pointer]))
(define-cpointer-type _CERT_CONTEXT _sCERT_CONTEXT-pointer)

(define-syntax-rule (_wfun . parts) (_fun #:abi 'stdcall . parts))

(define-crypt CertCloseStore
  (_wfun _CERTSTORE (_DWORD = 0) -> _int)
  #:wrap (deallocator))
(define-crypt CertOpenSystemStoreW
  (_wfun (_pointer = #f) _string/utf-16 -> _CERTSTORE/null)
  #:wrap (allocator CertCloseStore))
(define-crypt CertEnumCertificatesInStore
  (_wfun _CERTSTORE _CERT_CONTEXT/null
         -> _CERT_CONTEXT/null))

(define (CERT_CONTEXT->X509 c)
  (let* ([len (sCERT_CONTEXT-certEncodedLen c)]
         [data (sCERT_CONTEXT-certEncoded c)]
         [buf (make-bytes len)])
    (memcpy buf data len)
    (d2i_X509 buf)))

;; FIXME: also load CRLs?

(define (load-win32-root-certificates who ssl-ctx storename try?)
  (define cstore (CertOpenSystemStoreW storename))
  (cond [cstore
         (define xstore (SSL_CTX_get_cert_store ssl-ctx))
         (let loop ([curr-c #f])
           (define c (CertEnumCertificatesInStore cstore curr-c))
           (when c
             (let ([x509 (CERT_CONTEXT->X509 c)])
               (cond [x509
                      ;; FIXME: check result for errors
                      (X509_STORE_add_cert xstore x509)]
                     [try? (void)]
                     [else
                      (CertCloseStore cstore)
                      (error who "retrieved invalid certificate from store: ~e" storename)])
               (loop c))))
         (CertCloseStore cstore)
         (void)]
        [try? (void)]
        [else
         ;; FIXME: get error using GetLastError (atomically)
         (error who "failed to open certificate store: ~e" storename)]))
