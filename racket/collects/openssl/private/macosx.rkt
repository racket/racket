;; Support for loading root cerficates from Mac OS keychains.

#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         ffi/unsafe/define
         "add-cert.rkt")
(provide load-macosx-keychain)

;; TO DO:
;; - better error handling
;; - reliable mem management
;; - alternatives to deprecated functions

;; Alternative: generate PEM file with
;; security export -k /System/Library/Keychains/... -t certs -f pemseq -o foo.pem

(define libcf
  (case (system-type 'os*)
    [(macosx darwin) (ffi-lib "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation")]
    [else #f]))

(define-ffi-definer define-cf libcf
  #:default-make-fail make-not-available)

(define _fourchar _uint32)
(define _OSStatus _sint32)
(define _CFIndex _slong)

(define-cpointer-type _CFDataRef)

(define-cf CFRelease (_fun _pointer -> _void))

(define-cf CFDataGetLength
  (_fun _CFDataRef -> _CFIndex))

(define-cf CFDataGetBytePtr
  (_fun _CFDataRef -> _pointer))

(define (CFData->bytes data)
  (let* ([len (CFDataGetLength data)]
         [buf (make-bytes len)]
         [data-ptr (CFDataGetBytePtr data)])
    (memcpy buf data-ptr len)
    buf))

(define-cpointer-type _CFArrayRef)

(define-cf CFArrayGetCount
  (_fun [theArray : _CFArrayRef]
        -> _CFIndex))

(define-cf CFArrayGetValueAtIndex
  (_fun [theArray : _CFArrayRef]
        [idx : _CFIndex]
        -> _pointer))

;; ----

(define libsec
  (case (system-type 'os*)
    [(macosx darwin) (ffi-lib "/System/Library/Frameworks/Security.framework/Security")]
    [else #f]))
(define-ffi-definer define-sec libsec
  #:default-make-fail make-not-available)

(define CSSM_DB_RECORDTYPE_APP_DEFINED_START #x80000000)
(define kSecCertificateItemClass
  (+ CSSM_DB_RECORDTYPE_APP_DEFINED_START #x1000))
(define kSecFormatX509Cert 9)

(define _SecExternalFormat _int)
(define-cpointer-type _SecKeychainRef)
(define-cpointer-type _SecKeychainSearchRef)
(define-cpointer-type _SecKeychainAttributeListRef)

(define-sec SecKeychainOpen
  (_fun _path
        (ref : (_ptr o _SecKeychainRef/null))
        -> (result : _OSStatus)
        -> (values result ref)))

(define-sec SecKeychainSearchCreateFromAttributes
  (_fun _SecKeychainRef/null ;; FIXME: or array of keychains
        _fourchar
        _SecKeychainAttributeListRef/null
        (ref : (_ptr o _SecKeychainSearchRef/null))
        -> (result : _OSStatus)
        -> (values result ref)))

(define-sec SecKeychainSearchCopyNext ;; deprecated in 10.7
  (_fun _SecKeychainSearchRef
        (next : (_ptr o _id))
        -> (result : _OSStatus)
        -> (values result next)))

(define item-export-type
  (_fun (item) ::
        (item : _id)  ;; FIXME: SecCertificateRef or array
        (_SecExternalFormat = kSecFormatX509Cert) ;; DER
        (_int = 0)
        (_pointer = #f)
        (ref : (_ptr o _CFDataRef))
        -> (result : _OSStatus)
        -> (values result ref)))

(define-sec SecKeychainItemExport #| deprecated in 10.7 |# item-export-type)
(define-sec SecItemExport #|since 10.7|# item-export-type
  #:fail (lambda () SecKeychainItemExport))

(define-sec SecTrustCopyAnchorCertificates
  (_fun [anchors : (_ptr o _CFArrayRef)]
        -> (status : _OSStatus)
        -> (values status anchors)))

;; ----

(define (load-macosx-keychain who ssl-ctx path try?)
  (define ders
    (cond [path (keychain-path->ders who path try?)]
          [else (trust-anchors->ders who try?)]))
  (define xstore (SSL_CTX_get_cert_store ssl-ctx))
  (for ([der (in-list ders)])
    (let ([x509 (d2i_X509 der)])
      (cond [x509
             ;; FIXME: check result for errors (?)
             (X509_STORE_add_cert xstore x509)]
            [try? (void)]
            [else
             (error who "retrieved invalid certificate from keychain: ~e" path)]))))

(define (trust-anchors->ders who try?)
  (define-values (status roots) (SecTrustCopyAnchorCertificates))
  (cond [(= 0 status)
         (define len (CFArrayGetCount roots))
         (begin0 (for/list ([i (in-range len)])
                   (define cert (CFArrayGetValueAtIndex roots i))
                   (cert->der (cast cert _pointer _id)))
           (CFRelease roots))]
        [try? null]
        [else (error who "unable to retrieve Mac OS trust anchors")]))

(define (keychain-path->ders who path try?)
  (define path* (path->complete-path (cleanse-path path)))
  (define-values (status keychain)
    (SecKeychainOpen path*))
  (begin0 (cond [(= status 0)
                 (keychain->ders who keychain try?)]
                [try? null]
                [else
                 (error who "failed to open keychain: ~e" path)])
    (CFRelease keychain)))

(define (keychain->ders who keychain try?)
  (define-values (status search)
    (SecKeychainSearchCreateFromAttributes keychain kSecCertificateItemClass #f))
  (begin0 (cond [(= status 0)
                 (keychain-search->ders who search try?)]
                [try? null]
                [else (error "internal error: failed to open keychain search")])
    (CFRelease search)))

(define (keychain-search->ders who search try?)
  (let loop ()
    (define-values (status next)
      (SecKeychainSearchCopyNext search))
    (cond [(= status 0)
           (define der (cert->der next))
           (CFRelease next)
           (cons der (loop))]
          ;; FIXME: other error codes?
          [else null])))

(define (cert->der item)
  (define-values (status data) (SecItemExport item))
  (begin0 (CFData->bytes data)
    (CFRelease data)))
