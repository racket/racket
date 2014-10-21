#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         "add-cert.rkt"
         ffi/winapi)

;; Support for loading root cerficates from Windows certificate store.

(provide load-win32-store)

;; -- Windows CryptoAPI

(define crypt-lib
  (case (system-type)
    ((windows) (ffi-lib "crypt32.dll"))
    (else #f)))
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

(define-syntax-rule (_wfun . parts) (_fun #:abi winapi . parts))

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

(define (load-win32-store who ssl-ctx storename try?)
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
