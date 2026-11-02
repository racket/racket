#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         ffi/unsafe/alloc
         ffi/unsafe/define
         "verify.rkt")
(provide (protect-out (all-defined-out)))

;; Docs for SecTrustEvaluateWithError say:
;; > Don’t call SecTrustEvaluateWithError(_:_:) from your app’s main run loop
;; > because it might require network access to fetch intermediate certificates,
;; > or to perform revocation checking. To perform evaluation asynchronously, use
;; > SecTrustEvaluateAsyncWithError(_:_:_:) instead.

;; Options:
;; 1. Use SecTrustEvaluateAsyncWithError w/ network access enabled.
;;    + Recommended by docs, results probably most consistent with other apps.
;;    - This involves much more complexity, and is probably still blocking from
;;      Racket's point of view, since verifications happens in a callback from
;;      an OpenSSL FFI call.
;; 2. Use SecTrustEvaluateWithError w/ network access disabled.
;;    - Connections that succeed in Safari might fail in Racket because of
;;      missing intermediate certs. Results may be nondeterministic; verification
;;      might succeed or fail depending on whether intemediate certs are cached.
;;    + Simpler to implement.
;;    + Results will probably be consistent with previous Racket behavior, since
;;      OpenSSL does not automatically download missing intermediate certs:
;;      https://github.com/openssl/openssl/issues/5168.
;; 3. Ignore advice, use SecTrustEvaluateWithError w/ network access enabled.
;;    - Will block Racket thread. Network timeout doesn not appear configurable.
;;    + Maybe could mitigate by moving connect/verify work to parallel thread.

;; For now, this module implements option 2: verify without consulting network.

(define _Boolean _bool)

;; ----------------------------------------

(define libcf
  (case (system-type 'os*)
    [(macosx darwin ios)
     (ffi-lib "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation")]
    [else #f]))

(define-ffi-definer define-cf libcf
  #:default-make-fail make-not-available)

(define _CFTypeRef _pointer) ;; _id ??
(define _CFIndex _slong)

(define-cpointer-type _CFAllocatorRef)
(define-cpointer-type _CFArrayCallBacks)
(define-cpointer-type _CFErrorRef)

(define-cf CFRelease (_fun _pointer -> _void)
  #:wrap (deallocator))

(define-cpointer-type _CFStringRef)

(define _CFStringEncoding _uint32)
(define kCFStringEncodingUTF8 #x08000100)

(define-cf CFStringCreateWithBytes
  (_fun [allocator : _CFAllocatorRef/null = #f]
        [buf : _pointer]
        [buflen : _CFIndex]
        [encoding : _CFStringEncoding = kCFStringEncodingUTF8]
        -> _CFStringRef/null)
  #:wrap (allocator CFRelease))

(define (string->CFString s)
  (define buf (string->bytes/utf-8 s))
  (CFStringCreateWithBytes buf (bytes-length buf)))

(define-cpointer-type _CFDataRef)

(define-cf CFDataCreate
  (_fun [allocator : _CFAllocatorRef/null = #f]
        [data : #;const _pointer]
        [length : _CFIndex]
        -> _CFDataRef)
  #:wrap (allocator CFRelease))

(define (bytes->CFData data)
  (CFDataCreate data (bytes-length data)))

(define-cpointer-type _CFArrayRef)

(define-cf CFArrayCreate
  (_fun [allocator : _CFAllocatorRef/null = #f]
        [elems : _pointer]
        [numElems : _CFIndex]
        [callbacks : #;const _CFArrayCallBacks/null]
        -> _CFArrayRef))

;; Note: need &kCFTypeArrayCallbacks:
(define kCFTypeArrayCallBacks
  (cast (ffi-obj-ref 'kCFTypeArrayCallBacks libcf)
        _pointer _CFArrayCallBacks/null))

;; ----------------------------------------

(define libsec
  (case (system-type 'os*)
    [(macosx darwin) (ffi-lib "/System/Library/Frameworks/Security.framework/Security")]
    [else #f]))
(define-ffi-definer define-sec libsec
  #:default-make-fail make-not-available)

(define-cpointer-type _SecCertificateRef)
(define-cpointer-type _SecTrustRef)
(define-cpointer-type _SecPolicyRef)
(define _SecTrustResultType _uint32)
(define _OSStatus _sint32)
(define errSecSuccess 0)

(define-sec SecCertificateCreateWithData
  (_fun [allocator : _CFAllocatorRef/null = #f]
        [data : _CFDataRef]
        -> _SecCertificateRef/null)
  #:wrap (allocator CFRelease))

(define-sec SecTrustCreateWithCertificates
  (_fun [certificates : _CFTypeRef]
        [policies : _CFTypeRef]
        [trust : (_ptr io _SecTrustRef/null) = #f]
        -> [status : _OSStatus]
        -> (values status trust)))

(define-sec SecTrustSetAnchorCertificates
  (_fun [trust : _SecTrustRef]
        [anchorCertificates : _CFArrayRef]
        -> _OSStatus))

(define-sec SecTrustSetAnchorCertificatesOnly
  (_fun [trust : _SecTrustRef]
        [anchorCertificatesOnly : _Boolean]
        -> _OSStatus))

(define-sec SecTrustSetPolicies
  (_fun [trust : _SecTrustRef]
        [policies : _CFTypeRef]
        -> _OSStatus))

(define-sec SecTrustSetNetworkFetchAllowed
  (_fun [trust : _SecTrustRef]
        [allowFetch : _Boolean]
        -> _OSStatus))

;; Since Mac OS 10.14
(define-sec SecTrustEvaluateWithError
  (_fun [trust : _SecTrustRef]
        ;; I haven't found an enumeration of possible error results for this
        ;; function, so no point in fetching error info.
        [err : _pointer = #f] ;; (_ptr io _CFErrorRef/null)
        -> [ok? : _bool]
        ;; Returns true if the certificate is trusted.
        -> ok?))

;; Since Mac OS 10.15
;; Must *call* from same dispatch queue as `queue` parameter.
;; (define-sec SecTrustEvaluateAsyncWithError
;;   (_fun [trust : _SecTrustRef]
;;         [queue : _dispatch_queue_t]
;;         [result : _SecTrustWithErrorCallback]
;;         -> _OSStatus))
;; (define _SecTrustWithErrorCallback
;;   (_fun [trust : _SecTrustRef]
;;         [ok? : _bool]
;;         [err : _CFErrorRef]
;;         -> _void))

(define-sec SecTrustGetTrustResult
  (_fun [trust : _SecTrustRef]
        [result : (_ptr o _SecTrustResultType)]
        -> [s : _OSStatus]
        -> (values s result)))

(define-sec SecPolicyCreateSSL
  (_fun [server : _Boolean]
        [hostname : _CFStringRef]
        -> _SecPolicyRef))

;; ============================================================

(define trust-error-mapping
  `(
    ;; There is an unknown critical extension flag.
    (-67603 errSecUnknownCriticalExtensionFlag ,X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION)
    ;; The certificate cannot operate.
    ;;(-67817 errSecCertificateCannotOperate _)
    ;; An expired certificate was detected.
    (-67818 errSecCertificateExpired ,X509_V_ERR_CERT_HAS_EXPIRED)
    ;; The certificate is not yet valid.
    (-67819 errSecCertificateNotValidYet ,X509_V_ERR_CERT_NOT_YET_VALID)
    ;; The certificate was revoked.
    (-67820 errSecCertificateRevoked ,X509_V_ERR_CERT_REVOKED)
    ;; The certificate was suspended.
    ;;(-67821 errSecCertificateSuspended _)
    ;; The certificate authority is not valid.
    (-67826 errSecInvalidCertAuthority ,X509_V_ERR_CERT_UNTRUSTED) ; ??
    ;; An invalid certificate group was detected.
    ;;(-67691 errSecInvalidCertificateGroup _)
    ;; An invalid certificate reference was detected.
    ;;(-67690 errSecInvalidCertificateRef _)
    ;; The requested name isn’t allowed for this certificate.
    ;;(?? errSecCertificateNameNotAllowed _)
    ;; The requested policy isn’t allowed for this certificate.
    ;;(?? errSecCertificatePolicyNotAllowed _)
    ;; The validity period in the certificate exceeds the maximum allowed period.
    ;;(?? errSecCertificateValidityPeriodTooLong _)
    ))

;; set-trust-error : OSStatus _ -> Void
(define (set-trust-error s set-error)
  (cond [(assoc s trust-error-mapping)
         => (lambda (e) (set-error 0 (caddr e)))]
        [else (void)]))

;; ============================================================

;; der->cert : Bytes -> CertificateRef
(define (der->cert der)
  (define data (bytes->CFData der))
  (begin0 (SecCertificateCreateWithData data)
    (CFRelease data)))

;; make-ref-array : (Listof CFTypeRef) -> CFArrayRef
(define (make-ref-array refs)
  (define len (length refs))
  (define elems (malloc _pointer len 'raw))
  (for ([i (in-range len)]
        [ref (in-list refs)])
    (ptr-set! elems _pointer i ref))
  (begin0 (CFArrayCreate elems len kCFTypeArrayCallBacks)
    (free elems)))

;; make-trust : String/#f (Listof Bytes) _ -> (U SecTrustRef #f)
(define (make-trust hostname cert-ders [set-error void])
  (define hostname-ref (string->CFString hostname))
  (define policy-ref
    (begin0 (SecPolicyCreateSSL #t hostname-ref)
      (CFRelease hostname-ref)))
  (define cert-array
    (let ([certs (map der->cert cert-ders)])
      (begin0 (make-ref-array certs)
        (map CFRelease certs))))
  (define-values (s1 trust)
    (begin0 (SecTrustCreateWithCertificates cert-array policy-ref)
      (CFRelease cert-array)
      (CFRelease policy-ref)))
  (cond [(= s1 errSecSuccess)
         (define s2 (SecTrustSetNetworkFetchAllowed trust #f))
         (cond [(= s2 errSecSuccess)
                trust]
               [else
                (CFRelease trust)
                (set-trust-error s2 set-error)
                #f])]
        [else
         (when trust (CFRelease trust)) ;; impossible?
         (set-trust-error s1 set-error)
         #f]))

(define (macos-verify purpose hostname cert-der intermediate-ders [set-error void])
  (define trust (make-trust hostname (cons cert-der intermediate-ders) set-error))
  (and trust (SecTrustEvaluateWithError trust)))

(define macos-verify-callback (make-verify-callback macos-verify))
(define (get-macos-verify-callback) macos-verify-callback)
