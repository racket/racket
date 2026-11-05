#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         ffi/winapi
         "verify.rkt")
(provide (protect-out (all-defined-out)))

(define _BOOL _bool)
(define _LONG _long)
(define _DWORD _uint32)
(define _GUID (_array _int32 4))

(define (string->bytes/utf16 s)
  (define conv (bytes-open-converter "platform-UTF-8" "platform-UTF-16"))
  (define-values (u16bs nb status) (bytes-convert conv (string->bytes/utf-8 s)))
  (bytes-close-converter conv)
  (cond [(eq? status 'complete) u16bs]
        [else (error 'string->bytes/utf16 "failed: ~e" s)]))

(define-syntax-rule (_wfun . parts) (_fun #:abi winapi . parts))

(define (unmoving bs)
  (define NTLEN 2) ;; >=2 so we can use with WCHAR*
  (define bslen (bytes-length bs))
  (define mem (malloc (+ NTLEN (bytes-length bs)) 'raw))
  (memcpy mem 0 bs 0 bslen)
  (for ([offset (in-range bslen (+ bslen NTLEN))])
    (ptr-set! mem _byte offset 0))
  mem)

(define (unmoving-array . vs)
  (define mem (malloc (max 1 (length vs)) _pointer 'raw))
  (for ([v (in-list vs)] [i (in-naturals)])
    (ptr-set! mem _pointer i v))
  mem)

;; ============================================================

(define crypt-lib
  (case (system-type)
    ((windows) (ffi-lib "crypt32.dll"))
    (else #f)))
(define-ffi-definer define-crypt crypt-lib
  #:default-make-fail make-not-available)

(define-cpointer-type _HCERTCHAINENGINE)
(define-cpointer-type _FILETIME*)

;; ----------------------------------------

(define-cpointer-type _HCERTSTORE)

(define-crypt CertCloseStore
  (_fun [certStore : _HCERTSTORE]
        [flags : _DWORD = 0]
        -> _void)
  #:wrap (deallocator))

(define-crypt CertOpenStore
  ;; specialized to in-memory store
  (_fun [storeProvider : _intptr = CERT_STORE_PROV_MEMORY]
        [encodingType : _DWORD = 0]
        [cryptProv : _pointer #;HCRYPTPROV_LEGACY = #f]
        [flags : _DWORD = 0]
        [para : #;const _pointer = #f]
        -> _HCERTSTORE)
  #:wrap (allocator CertCloseStore))

(define CERT_STORE_PROV_MEMORY 2)

;; CertAddCertificateContextToStore

(define-crypt CertAddEncodedCertificateToStore
  (_fun [certStore : _HCERTSTORE]
        [certEncodingType : _DWORD = X509_ASN_ENCODING]
        [certEncoded : #;const _bytes]
        [certEncodedLen : _DWORD = (bytes-length certEncoded)]
        [addDisposition : _DWORD = CERT_STORE_ADD_ALWAYS]
        [certContext : _pointer = #f]
        -> _BOOL))

(define X509_ASN_ENCODING   #x00000001)

(define CERT_STORE_ADD_USE_EXISTING     2)
(define CERT_STORE_ADD_REPLACE_EXISTING 3)
(define CERT_STORE_ADD_ALWAYS           4)

;; ----------------------------------------

(define-cpointer-type _CERT_CONTEXT*)

(define-crypt CertFreeCertificateContext
  (_fun [certContext : _CERT_CONTEXT*] -> _BOOL)
  #:wrap (deallocator))

(define-crypt CertCreateCertificateContext
  (_fun [dwCertEncodingType : _DWORD = X509_ASN_ENCODING]
        [pbCertEncoded : #;const _bytes]
        [cbCertEncoded : _DWORD = (bytes-length pbCertEncoded)]
        -> _CERT_CONTEXT*/null)
  #:wrap (allocator CertFreeCertificateContext))

(define (bytes->CERT_CONTEXT der)
  (CertCreateCertificateContext der))

;; ----------------------------------------

;; https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/ns-wincrypt-cert_trust_status

(define-cstruct _CERT_TRUST_STATUS
  ([errorStatus _DWORD]
   [infoStatus  _DWORD]))
(define _CERT_TRUST_STATUS* _CERT_TRUST_STATUS-pointer)

(define CERT_TRUST_NO_ERROR #x00000000)

(define trust-error-mapping
  `(;; reordered:
    (#x04000000 CERT_TRUST_IS_EXPLICIT_DISTRUST ,X509_V_ERR_CERT_REJECTED)
    ;; ----
    (#x00000001 CERT_TRUST_IS_NOT_TIME_VALID ,X509_V_ERR_CERT_HAS_EXPIRED) ;; or NOT_YET_VALID
    (#x00000004 CERT_TRUST_IS_REVOKED ,X509_V_ERR_CERT_REVOKED)
    (#x00000008 CERT_TRUST_IS_NOT_SIGNATURE_VALID ,X509_V_ERR_CERT_SIGNATURE_FAILURE)
    (#x00000010 CERT_TRUST_IS_NOT_VALID_FOR_USAGE ,X509_V_ERR_INVALID_PURPOSE)
    (#x00000020 CERT_TRUST_IS_UNTRUSTED_ROOT ,X509_V_ERR_CERT_UNTRUSTED)
    ;(#x00000040 CERT_TRUST_REVOCATION_STATUS_UNKNOWN)
    (#x00000080 CERT_TRUST_IS_CYCLIC ,X509_V_ERR_PATH_LOOP)
    (#x00000100 CERT_TRUST_INVALID_EXTENSION ,X509_V_ERR_INVALID_EXTENSION)
    (#x00000200 CERT_TRUST_INVALID_POLICY_CONSTRAINTS ,X509_V_ERR_INVALID_POLICY_EXTENSION)
    ;(#x00000400 CERT_TRUST_INVALID_BASIC_CONSTRAINTS)
    (#x00000800 CERT_TRUST_INVALID_NAME_CONSTRAINTS ,X509_V_ERR_INVALID_EXTENSION)
    (#x00001000 CERT_TRUST_HAS_NOT_SUPPORTED_NAME_CONSTRAINT ,X509_V_ERR_INVALID_EXTENSION)
    (#x00002000 CERT_TRUST_HAS_NOT_DEFINED_NAME_CONSTRAINT  ,X509_V_ERR_INVALID_EXTENSION)
    (#x00004000 CERT_TRUST_HAS_NOT_PERMITTED_NAME_CONSTRAINT ,X509_V_ERR_INVALID_EXTENSION)
    (#x00008000 CERT_TRUST_HAS_EXCLUDED_NAME_CONSTRAINT ,X509_V_ERR_INVALID_EXTENSION)
    ;(#x01000000 CERT_TRUST_IS_OFFLINE_REVOCATION)
    ;(#x02000000 CERT_TRUST_NO_ISSUANCE_CHAIN_POLICY)
    ;(#x08000000 CERT_TRUST_HAS_NOT_SUPPORTED_CRITICAL_EXT)
    ;(#x00100000 CERT_TRUST_HAS_WEAK_SIGNATURE)
    ;; only for chains:
    ;(#x00010000 CERT_TRUST_IS_PARTIAL_CHAIN)
    ;(#x00020000 CERT_TRUST_CTL_IS_NOT_TIME_VALID)
    ;(#x00040000 CERT_TRUST_CTL_IS_NOT_SIGNATURE_VALID)
    ;(#x00080000 CERT_TRUST_CTL_IS_NOT_VALID_FOR_USAGE)
    ))

(define (lookup-code-mapping code table)
  (for/or ([e (in-list table)])
    (if (zero? (bitwise-and code (car e))) #f (caddr e))))

(define (set-chain-trust-error depth trust set-error)
  (define errcode (CERT_TRUST_STATUS-errorStatus trust))
  (cond [(= errcode X509_V_OK) #f]
        [(lookup-code-mapping errcode trust-error-mapping)
         => (lambda (code) (begin (set-error depth code) #t))]
        [else #f]))

;; ----------------------------------------

(define-cstruct _CERT_ENHKEY_USAGE
  ([count   _DWORD]
   [oids    _pointer #| to array of strings |#]))

(define szOID_PKIX_KP_SERVER_AUTH  #"1.3.6.1.5.5.7.3.1")
(define szOID_PKIX_KP_CLIENT_AUTH  #"1.3.6.1.5.5.7.3.2")

;; "server-auth" means check cert provided by server; performed by client
(define server-auth-ekus
  (unmoving-array (unmoving szOID_PKIX_KP_SERVER_AUTH)))
(define client-auth-ekus
  (unmoving-array (unmoving szOID_PKIX_KP_CLIENT_AUTH)))
(define dont-gc (list server-auth-ekus client-auth-ekus))

(define (empty-CERT_ENHKEY_USAGE)
  (make-CERT_ENHKEY_USAGE 0 #f))
(define (server-auth-CERT_ENHKEY_USAGE)
  (make-CERT_ENHKEY_USAGE 1 server-auth-ekus))
(define (client-auth-CERT_ENHKEY_USAGE)
  (make-CERT_ENHKEY_USAGE 1 client-auth-ekus))

(define-cstruct _CERT_USAGE_MATCH
  ([type    _DWORD]
   [usage   _CERT_ENHKEY_USAGE]))

(define USAGE_MATCH_TYPE_AND #x00000000)
(define USAGE_MATCH_TYPE_OR  #x00000001)

(define (new-CERT_USAGE_MATCH usage)
  (make-CERT_USAGE_MATCH USAGE_MATCH_TYPE_AND usage))

;; ----------------------------------------

;; https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/ns-wincrypt-cert_chain_para

(define-cstruct _CERT_CHAIN_PARA
  ([size                    _DWORD]
   [requestedUsage          _CERT_USAGE_MATCH]
   [requestedIssuancePolicy _CERT_USAGE_MATCH]
   [retrievalTimeoutMS      _DWORD]
   [checkRevocationFreshnessTime _BOOL]
   [revocationFreshnessTime _DWORD]
   [cacheResync             _pointer #;LPFILETIME]
   [strongSignPara          _pointer #;CERT_STRONG_SIGN_PARA]
   [strongSignFlags         _DWORD]))
(define _CERT_CHAIN_PARA* _CERT_CHAIN_PARA-pointer)

(define (new-CERT_CHAIN_PARA purpose [retrievalTimeoutMS (* 10 1000)])
  (define para (cast (malloc _CERT_CHAIN_PARA 'atomic) _pointer _CERT_CHAIN_PARA*))
  (memset para 0 1 _CERT_CHAIN_PARA)
  (set-CERT_CHAIN_PARA-size! para (ctype-sizeof _CERT_CHAIN_PARA))
  (define requestedUsage
    (case purpose
      [(server-auth) (new-CERT_USAGE_MATCH (server-auth-CERT_ENHKEY_USAGE))]
      [(client-auth) (new-CERT_USAGE_MATCH (client-auth-CERT_ENHKEY_USAGE))]))
  (set-CERT_CHAIN_PARA-requestedUsage! para requestedUsage)
  (set-CERT_CHAIN_PARA-retrievalTimeoutMS! para retrievalTimeoutMS)
  para)

;; ----------------------------------------

(define-cstruct _CERT_CHAIN_ELEMENT
  ([size        _DWORD]
   [certContext _CERT_CONTEXT*]
   [trustStatus _CERT_TRUST_STATUS]
   ;; incomplete
   ))

(define (set-chain-element-error depth elem set-error)
  (define trust (CERT_CHAIN_ELEMENT-trustStatus elem))
  (set-chain-trust-error depth trust set-error))

(define-cstruct _CERT_SIMPLE_CHAIN
  ([size            _DWORD]
   [trustStatus     _CERT_TRUST_STATUS]
   [cElement        _DWORD]
   [elements        _pointer #;CERT_CHAIN_ELEMENT**]
   ;; incomplete
   ))
(define _CERT_SIMPLE_CHAIN* _CERT_SIMPLE_CHAIN-pointer)

(define (set-simple-chain-error sc set-error)
  (define trust (CERT_SIMPLE_CHAIN-trustStatus sc))
  (cond [(= (CERT_TRUST_STATUS-errorStatus trust) CERT_TRUST_NO_ERROR) #f]
        [else
         (define elements (CERT_SIMPLE_CHAIN-elements sc))
         (for/or ([depth (in-range (CERT_SIMPLE_CHAIN-cElement sc))])
           (define elem (ptr-ref elements _CERT_CHAIN_ELEMENT-pointer depth))
           (set-chain-element-error depth elem set-error))]))

;; ----------------------------------------

(define-cstruct _CERT_CHAIN_CONTEXT
  ([size            _DWORD]
   [trustStatus     _CERT_TRUST_STATUS]
   [cChains         _DWORD]
   [chains          _pointer #;CERT_SIMPLE_CHAIN**]
   [cLowerQualityChainContext   _DWORD]
   [lowerQualityChainContext    _pointer #;CERT_CHAIN_CONTEXT**]
   [hasRevocationFreshnessTime  _BOOL]
   [revocationFreshnessTime     _DWORD]
   [createFlags     _DWORD]
   [chainId         _GUID]))
(define _CERT_CHAIN_CONTEXT* _CERT_CHAIN_CONTEXT-pointer)
(define _CERT_CHAIN_CONTEXT*/null _CERT_CHAIN_CONTEXT-pointer/null)

(define (set-chain-error chain set-error)
  (or (cond [(>= (CERT_CHAIN_CONTEXT-cChains chain) 1)
             (define scs (CERT_CHAIN_CONTEXT-chains chain))
             (define sc (ptr-ref scs _CERT_SIMPLE_CHAIN* 0))
             (set-simple-chain-error sc set-error)]
            [else #f])
      (set-chain-trust-error 0 (CERT_CHAIN_CONTEXT-trustStatus chain) set-error)))

;; ----------------------------------------

(define-crypt CertFreeCertificateChain
  (_fun [chainContext : _CERT_CHAIN_CONTEXT*] -> _void)
  #:wrap (deallocator))

(define-crypt CertGetCertificateChain
  (_fun [chainEngine  : _HCERTCHAINENGINE/null = #f]
        [certContext  : _CERT_CONTEXT*]
        [filetime     : _FILETIME*/null = #f]
        [addedStore   : _HCERTSTORE/null]
        [chainPara    : _CERT_CHAIN_PARA*]
        [flags        : _DWORD]
        [reserved     : _pointer = #f]
        [chainContext : (_ptr io _CERT_CHAIN_CONTEXT*/null) = #f]
        -> [r : _BOOL]
        -> (if r chainContext #f))
  #:wrap (allocator CertFreeCertificateChain))

(define CERT_CHAIN_CACHE_END_CERT                   #x00000001)
(define CERT_CHAIN_CACHE_ONLY_URL_RETRIEVAL         #x00000004)
(define CERT_CHAIN_DISABLE_AUTH_ROOT_AUTO_UPDATE    #x00000100)
(define CERT_CHAIN_TIMESTAMP_TIME                   #x00000200)
(define CERT_CHAIN_DISABLE_AIA                      #x00002000)
(define CERT_CHAIN_REVOCATION_CHECK_OCSP_CERT       #x04000000)
(define CERT_CHAIN_REVOCATION_ACCUMULATIVE_TIMEOUT  #x08000000)
(define CERT_CHAIN_REVOCATION_CHECK_END_CERT        #x10000000)
(define CERT_CHAIN_REVOCATION_CHECK_CHAIN           #x20000000)
(define CERT_CHAIN_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT #x40000000)
(define CERT_CHAIN_REVOCATION_CHECK_CACHE_ONLY      #x80000000)

;; ============================================================

(define-cstruct _CERT_CHAIN_POLICY_PARA
  ([size  _DWORD]
   [flags _DWORD]
   [extra _pointer]) ;; use SSL_EXTRA_CERT_CHAIN_POLICY_PARA
  #:malloc-mode 'atomic-interior)
(define _CERT_CHAIN_POLICY_PARA* _CERT_CHAIN_POLICY_PARA-pointer)

(define CERT_CHAIN_POLICY_IGNORE_END_REV_UNKNOWN_FLAG           #x00000100)
(define CERT_CHAIN_POLICY_IGNORE_CTL_SIGNER_REV_UNKNOWN_FLAG    #x00000200)
(define CERT_CHAIN_POLICY_IGNORE_CA_REV_UNKNOWN_FLAG            #x00000400)
(define CERT_CHAIN_POLICY_IGNORE_ROOT_REV_UNKNOWN_FLAG          #x00000800)
(define CERT_CHAIN_POLICY_IGNORE_ALL_REV_UNKNOWN_FLAGS
  (bitwise-ior CERT_CHAIN_POLICY_IGNORE_END_REV_UNKNOWN_FLAG
               CERT_CHAIN_POLICY_IGNORE_CTL_SIGNER_REV_UNKNOWN_FLAG
               CERT_CHAIN_POLICY_IGNORE_CA_REV_UNKNOWN_FLAG
               CERT_CHAIN_POLICY_IGNORE_ROOT_REV_UNKNOWN_FLAG))

(define (new-ssl-CERT_CHAIN_POLICY_PARA extra-policy)
  (define size (ctype-sizeof _CERT_CHAIN_POLICY_PARA))
  (define flags CERT_CHAIN_POLICY_IGNORE_ALL_REV_UNKNOWN_FLAGS)
  (make-CERT_CHAIN_POLICY_PARA size flags extra-policy))

;; ----------------------------------------

(define-cstruct _SSL_EXTRA_CERT_CHAIN_POLICY_PARA
  ([size        _DWORD]
   [authType    _DWORD]
   [checks      _DWORD]
   [serverName  _pointer #;WCHAR*])
  #:malloc-mode 'atomic-interior)

(define AUTHTYPE_CLIENT 1)
(define AUTHTYPE_SERVER 2)

(define (new-SSL_EXTRA_CERT_CHAIN_POLICY_PARA purpose hostname-mem)
  (make-SSL_EXTRA_CERT_CHAIN_POLICY_PARA
   (ctype-sizeof _SSL_EXTRA_CERT_CHAIN_POLICY_PARA)
   (case purpose
     [(server-auth) AUTHTYPE_SERVER]
     [(client-auth) AUTHTYPE_CLIENT])
   0
   hostname-mem))

;; ----------------------------------------

(define-cstruct _CERT_CHAIN_POLICY_STATUS
  ([size          _DWORD]
   [error         _DWORD]
   [chainIndex    _LONG]
   [elementIndex  _LONG]
   [extraPolicyStatus _pointer]))
(define _CERT_CHAIN_POLICY_STATUS* _CERT_CHAIN_POLICY_STATUS-pointer)

(define (new-CERT_CHAIN_POLICY_STATUS)
  (define size (ctype-sizeof _CERT_CHAIN_POLICY_STATUS))
  (make-CERT_CHAIN_POLICY_STATUS size 0 0 0 #f))

(define policy-error-mapping
  `(;; The signature of the certificate cannot be verified.
    (#x80096004 TRUST_E_CERT_SIGNATURE ,X509_V_ERR_CERT_SIGNATURE_FAILURE)
    ;; The certificate or signature has been revoked.
    (#x80092010 CRYPT_E_REVOKED ,X509_V_ERR_CERT_REVOKED)
    ;; A certification chain processed correctly but terminated in a root
    ;; certificate that is not trusted by the trust provider.
    (#x800B0109 CERT_E_UNTRUSTEDROOT ,X509_V_ERR_CERT_UNTRUSTED)
    ;; The root certificate is a testing certificate, and policy settings
    ;; disallow test certificates.
    (#x800B010D CERT_E_UNTRUSTEDTESTROOT ,X509_V_ERR_CERT_UNTRUSTED)
    ;; A chain of certificates was not correctly created.
    ;;(#x800B010A CERT_E_CHAINING _)
    ;; The certificate is not valid for the requested usage.
    (#x800B0110 CERT_E_WRONG_USAGE ,X509_V_ERR_INVALID_PURPOSE)
    ;; A required certificate is not within its validity period.
    (#x800B0101 CERT_E_EXPIRED ,X509_V_ERR_CERT_HAS_EXPIRED) ;; or NOT_YET_VALID
    ;; The certificate has an invalid name. Either the name is not included in
    ;; the permitted list, or it is explicitly excluded.
    (#x800B0114 CERT_E_INVALID_NAME ,X509_V_ERR_EXCLUDED_VIOLATION) ;; or PERMITTED_VIOLATION
    ;; The certificate has an invalid policy.
    ;;(#x800B0113 CERT_E_INVALID_POLICY _)
    ;; The basic constraints of the certificate are not valid, or they are missing.
    ;;(#x80096019 TRUST_E_BASIC_CONSTRAINTS _)
    ;; The certificate is being used for a purpose other than the purpose
    ;; specified by its CA.
    (#x800B0105 CERT_E_CRITICAL ,X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION)
    ;; The validity periods of the certification chain do not nest correctly.
    ;;(#x800B0102 CERT_E_VALIDITYPERIODNESTING _)
    ;; The revocation function was unable to check revocation for the certificate.
    ;;(#x80092012 CRYPT_E_NO_REVOCATION_CHECK _)
    ;; The revocation function was unable to check revocation because the
    ;; revocation server was offline.
    ;;(#x80092013 CRYPT_E_REVOCATION_OFFLINE _)
    ;; The certificate's CN name does not match the passed value.
    (#x800B010F CERT_E_CN_NO_MATCH ,X509_V_ERR_HOSTNAME_MISMATCH)
    ;; The certificate is being used for a purpose other than the purposes
    ;; specified by its CA.
    (#x800B0106 CERT_E_PURPOSE ,X509_V_ERR_CERT_REJECTED)
    ;; A certificate that can only be used as an end-entity is being used as a
    ;; CA or vice versa.
    (#x800B0103 CERT_E_ROLE ,X509_V_ERR_INVALID_PURPOSE)))

(define (set-verify-error status set-error)
  (define errcode (CERT_CHAIN_POLICY_STATUS-error status))
  ;; elementIndex can be -1 (whole chain), but OpenSSL needs nonnegative depth
  (define depth (max 0 (CERT_CHAIN_POLICY_STATUS-elementIndex status)))
  (cond [(assoc errcode policy-error-mapping)
         => (lambda (e) (set-error depth (caddr e)))]
        [else (void)]))

;; ----------------------------------------

(define-crypt CertVerifyCertificateChainPolicy
  (_fun [policyOID    : _intptr = CERT_CHAIN_POLICY_SSL]
        [chainContext : _CERT_CHAIN_CONTEXT*]
        [policyPara   : _CERT_CHAIN_POLICY_PARA*]
        [policyStatus : _CERT_CHAIN_POLICY_STATUS*
                      = (new-CERT_CHAIN_POLICY_STATUS)]
        -> [r : _BOOL] ;; indicates whether check performed, not whether passed
        -> (if r policyStatus #f)))

(define CERT_CHAIN_POLICY_SSL 4)

;; ============================================================

;; Recommendations from https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certgetcertificatechain:
;; - Only enable revocation checking for the end certificate.
;;   Set CERT_CHAIN_REVOCATION_CHECK_END_CERT.
;; - Enable network retrievals for CRLs, OCSP, AIA issuers and Windows platform CTL cabs
;;   and Third Party roots (default with above unless disabled by other flags).
;; - Enable accumulative timeout for CRL and OCSP network retrievals.
;;   Set CERT_CHAIN_REVOCATION_ACCUMULATIVE_TIMEOUT.
;; - Reduce the maximum time allowed for each network retrieval from 15 to 10 seconds.
;;   Set the dwUrlRetrievalTimeout field in the CERT_CHAIN_PARA to 10 * 1000 milliseconds.
;; - Ignore offline revocation errors.
;;   Set CERT_CHAIN_POLICY_IGNORE_ALL_REV_UNKNOWN_FLAGS in CERT_CHAIN_POLICY_PARA passed to
;;   CertVerifyCertificateChainPolicy(CERT_CHAIN_POLICY_SSL).
;; - Cache end certificate validation information.
;;   - Set the CERT_CHAIN_CACHE_END_CERT.

(define (get-cert-chain purpose cert-der untrusted-cert-ders)
  (define cert (bytes->CERT_CONTEXT cert-der))
  (define untrusted-store (CertOpenStore))
  (for ([untrusted-cert-der (in-list untrusted-cert-ders)])
    (CertAddEncodedCertificateToStore untrusted-store untrusted-cert-der))
  (define chain-para (new-CERT_CHAIN_PARA purpose))
  (define flags
    (bitwise-ior CERT_CHAIN_CACHE_END_CERT
                 CERT_CHAIN_REVOCATION_ACCUMULATIVE_TIMEOUT
                 CERT_CHAIN_REVOCATION_CHECK_END_CERT))
  (CertGetCertificateChain cert untrusted-store chain-para flags))

(define (verify-policy purpose hostname chain [set-error void])
  (define hostname-mem (unmoving (string->bytes/utf16 hostname)))
  (define extra-policy (new-SSL_EXTRA_CERT_CHAIN_POLICY_PARA purpose hostname-mem))
  (define policy-para (new-ssl-CERT_CHAIN_POLICY_PARA extra-policy))
  (define status (CertVerifyCertificateChainPolicy chain policy-para))
  (void/reference-sink hostname-mem)
  (void/reference-sink extra-policy)
  (void/reference-sink policy-para)
  (void/reference-sink dont-gc)
  (cond [status
         (define errorStatus (CERT_CHAIN_POLICY_STATUS-error status))
         (cond [(= errorStatus CERT_TRUST_NO_ERROR) #t]
               [else (begin (set-verify-error status set-error) #f)])]
        [else #f]))

(define (win32-verify purpose hostname cert-der untrusted-cert-ders [set-error void])
  (define chain (get-cert-chain purpose cert-der untrusted-cert-ders))
  ;; The chain's trustStatus might have errors, but some of those may be ignored
  ;; by the policy config (eg, unknown revocation status errors).
  (and chain (verify-policy purpose hostname chain set-error)))

(define win32-verify-callback (make-verify-callback win32-verify))
(define (get-win32-verify-callback) win32-verify-callback)
