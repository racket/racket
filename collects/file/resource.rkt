#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
	 ffi/winapi)

(provide get-resource
         write-resource)

(define _HKEY (_cpointer/null 'HKEY))

(define (const-hkey v)
  (cast (bitwise-ior v (arithmetic-shift -1 32)) _intptr _HKEY))

(define HKEY_CLASSES_ROOT   (const-hkey #x80000000))
(define HKEY_CURRENT_USER   (const-hkey #x80000001))
(define HKEY_LOCAL_MACHINE  (const-hkey #x80000002))
(define HKEY_USERS          (const-hkey #x80000003))
(define HKEY_CURRENT_CONFIG (const-hkey #x80000005))

(define REG_SZ 1)
(define REG_BINARY 3)
(define REG_DWORD 4)

(define (section->hkey who section)
  (cond
   [(equal? section "HKEY_CLASSES_ROOT")
    HKEY_CLASSES_ROOT]
   [(equal? section "HKEY_CURRENT_CONFIG")
    HKEY_CURRENT_CONFIG]
   [(equal? section "HKEY_CURRENT_USER")
    HKEY_CURRENT_USER]
   [(equal? section "HKEY_LOCAL_MACHINE")
    HKEY_LOCAL_MACHINE]
   [(equal? section "HKEY_USERS")
    HKEY_USERS]
   [(string? section) #f]
   [else
    (raise-type-error who "string" section)]))

(define advapi-dll (and (eq? (system-type) 'windows)
                     (ffi-lib "Advapi32.dll")))
(define kernel-dll (and (eq? (system-type) 'windows)
                        (ffi-lib "kernel32.dll")))

(define-ffi-definer define-advapi advapi-dll
  #:default-make-fail make-not-available)
(define-ffi-definer define-kernel kernel-dll
  #:default-make-fail make-not-available)

(define _LONG _long)
(define _DWORD _int32)
(define _REGSAM _DWORD)
(define _BOOL (make-ctype _int (lambda (v) (if v 1 0)) (lambda (v) (not (zero? v)))))

(define KEY_QUERY_VALUE #x1)
(define KEY_SET_VALUE   #x2)

(define ERROR_SUCCESS 0)

(define-advapi RegOpenKeyExW (_fun #:abi winapi
                                   _HKEY _string/utf-16 _DWORD _REGSAM (hkey : (_ptr o _HKEY))
                                   -> (r : _LONG)
                                   -> (and (= r ERROR_SUCCESS) hkey)))
(define-advapi RegCreateKeyExW (_fun #:abi winapi
                                     _HKEY _string/utf-16 (_DWORD = 0) 
                                     (_pointer = #f) ; class
                                     _DWORD ; options
                                     _REGSAM
                                     _pointer ; security
                                     (hkey : (_ptr o _HKEY))
                                     (_ptr o _DWORD) ; disposition
                                     -> (r : _LONG)
                                     -> (and (= r ERROR_SUCCESS) hkey)))

(define-advapi RegQueryValueExW (_fun #:abi winapi
                                      _HKEY _string/utf-16 (_pointer = #f)
                                      (type : (_ptr o _DWORD))
                                      _pointer (len : (_ptr io _DWORD))
                                      -> (r : _LONG)
                                      -> (if (= r ERROR_SUCCESS) 
                                             (values len type)
                                             (values #f #f))))
(define-advapi RegSetValueExW (_fun #:abi winapi
                                    _HKEY _string/utf-16 (_pointer = #f)
                                    _DWORD _pointer _DWORD
                                    -> (r : _LONG)
                                    -> (= r ERROR_SUCCESS)))

(define-advapi RegCloseKey (_fun #:abi winapi _HKEY -> _LONG))

(define-kernel WritePrivateProfileStringW (_fun #:abi winapi
                                                _string/utf-16 ; app
                                                _string/utf-16 ; key
                                                _string/utf-16 ; val
                                                _string/utf-16 ; filename
                                                -> _BOOL))
(define-kernel GetPrivateProfileStringW (_fun #:abi winapi
                                              _string/utf-16 ; app
                                              _string/utf-16 ; key
                                              _string/utf-16 ; default
                                              _pointer ; result
                                              _DWORD ; result size in wide chars
                                              _string/utf-16 ; filename
                                              -> _DWORD))

(define (file->ini f)
  (cond
   [(not f) (file->ini 
             (build-path (find-system-path 'home-dir) "mred.ini"))]
   [(string? f) (file->ini (string->path f))]
   [(path? f) (path->string (cleanse-path (path->complete-path f)))]))

(define (extract-sub-hkey file hkey entry op create-key?)
  (cond
   [(not (eq? 'windows (system-type))) (values #f #f)]
   [file (values #f #f)]
   [(regexp-match #rx"^(.*)\\\\+([^\\]*)$" entry)
    => (lambda (m)
         (let ([sub-hkey (RegOpenKeyExW hkey (cadr m) 0 op)]
               [sub-entry (caddr m)])
           (if (and (not sub-hkey)
                    create-key?)
               (values (RegCreateKeyExW hkey (cadr m) 0 op #f)
                       sub-entry)
               (values sub-hkey sub-entry))))]
   [else (values hkey entry)]))

(define (get-resource section entry [value #f] [file #f]
                      #:type [rtype (or (and (box? value)
                                             (or
                                              (and (exact-integer? (unbox value))
                                                   'integer)
                                              (and (bytes? (unbox value))
                                                   'bytes)))
                                        'string)])
  (define hkey (section->hkey 'get-resource section))
  (unless (string? entry)
    (raise-type-error 'get-resource "string" entry))
  (unless (or (not value)
              (and (box? value)
                   (let ([value (unbox value)])
                     (or (string? value) (bytes? value) (exact-integer? value)))))
    (raise-type-error 'get-resource "#f or box of string, byte string, or exact integer" value))
  (unless (or (not file)
              (path-string? file))
    (raise-type-error 'get-resource "path string or #f" file))
  (unless (memq rtype '(string bytes integer))
    (raise-type-error 'get-resource "'string, 'bytes, or 'integer" rtype))
  
  (define (to-rtype s)
    (let ([to-string (lambda (s)
                       (if (bytes? s)
                           (bytes->string/utf-8 s #\?)
                           s))])
      (cond
       [(eq? rtype 'string) (to-string s)]
       [(eq? rtype 'integer)
        (let ([n (string->number (to-string s))])
          (or (and n (exact-integer? n) n)
              0))]
       [else
        (if (string? s)
            (string->bytes/utf-8 s)
            s)])))

  (define-values (sub-hkey sub-entry)
    (extract-sub-hkey file hkey entry KEY_QUERY_VALUE #f))

  (cond
   [sub-hkey
    (begin0
     (let-values ([(len type) 
                   ;; Get size, first
                   (RegQueryValueExW sub-hkey sub-entry #f 0)])
       (and len
            (let ([s (make-bytes len)])
              (let-values ([(len2 type2) 
                            ;; Get value, now that we have a bytes string of the right size
                            (RegQueryValueExW sub-hkey sub-entry s len)])
                (and len2
                     (let ([r
                            ;; Unmarhsal according to requested type:
                            (let ([s (cond
                                      [(= type REG_SZ)
                                       (cast s _pointer _string/utf-16)]
                                      [(= type REG_DWORD)
                                       (number->string (ptr-ref s _DWORD))]
                                      [else
                                       s])])
                              (to-rtype s))])
                       (if (box? value)
                           (begin
                             (set-box! value r)
                             #t)
                           r)))))))
     (unless (eq? hkey sub-hkey)
       (RegCloseKey sub-hkey)))]
   [(eq? 'windows (system-type))
    (let* ([SIZE 1024]
           [dest (make-bytes (* SIZE 2) 0)]
           [DEFAULT "$$default"]
           [len (GetPrivateProfileStringW section entry DEFAULT
                                          dest SIZE
                                          (file->ini file))])
      (let ([s (cast dest _pointer _string/utf-16)])
        (and (not (equal? s DEFAULT))
             (let ([r (to-rtype s)])
               (if value
                   (begin
                     (set-box! value r)
                     #t)
                   r)))))]
   [else #f]))

(define (write-resource section entry value [file #f]
                        #:type [type 'string]
                        #:create-key? [create-key? #f])
  (define hkey (section->hkey 'write-resource section))
  (unless (string? entry)
    (raise-type-error 'write-resource "string" entry))
  (unless (or (string? value) (bytes? value) (exact-integer? value))
    (raise-type-error 'write-resource "string, byte string, or exact integer" value))
  (unless (or (not file)
              (path-string? file))
    (raise-type-error 'write-resource "path string or #f" file))
  (unless (memq type '(string bytes dword))
    (raise-type-error 'write-resource "'string, 'bytes, or 'dword" type))

  (define (to-string value)
    (cond
     [(exact-integer? value) (number->string value)]
     [(string? value) value]
     [else (bytes->string/utf-8 value #\?)]))

  (define-values (sub-hkey sub-entry)
    (extract-sub-hkey file hkey entry KEY_SET_VALUE create-key?))
  
  (cond
   [sub-hkey
    (begin0
     (let ([v (case type
                [(string) 
                 (to-utf-16 (to-string value))]
                [(bytes) 
                 (cond
                  [(exact-integer? value) 
                   (string->bytes/utf-8 (number->string value))]
                  [(string? value) (string->bytes/utf-8 value)]
                  [else value])]
                [(dword) 
                 (to-dword-ptr
                  (cond
                   [(exact-integer? value) value]
                   [(string? value) (string->number value)]
                   [(bytes? value) 
                    (string->number (bytes->string/utf-8 value #\?))]))])]
           [ty (case type
                 [(string) REG_SZ]
                 [(bytes) REG_BINARY]
                 [(dword) REG_DWORD])])
       (RegSetValueExW sub-hkey sub-entry ty v (bytes-length v)))
     (unless (eq? hkey sub-hkey)
       (RegCloseKey sub-hkey)))]
   [(eq? 'windows (system-type))
    (WritePrivateProfileStringW section entry (to-string value) (file->ini file))]
   [else #f]))

(define (to-utf-16 s)
  (let ([v (malloc _gcpointer)])
    (ptr-set! v _string/utf-16 s)
    (let ([p (ptr-ref v _gcpointer)])
      (let ([len (* 2 (+ 1 (utf-16-length s)))])
        (ptr-ref v (_bytes o len))))))

(define (utf-16-length s)
  (for/fold ([len 0]) ([c (in-string s)])
    (+ len
       (if ((char->integer c) . > . #xFFFF)
           2
           1))))

(define (to-dword-ptr v)
  (let ([v (if (and (exact-integer? v)
                    (<= (- (expt 2 31))
                        v
                        (sub1 (expt 2 31))))
               v
               0)])
    (let ([p (malloc _DWORD)])
      (ptr-set! p _DWORD v)
      (cast p _pointer (_bytes o (ctype-sizeof _DWORD))))))
