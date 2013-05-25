#lang racket/base
(require ffi/unsafe
         ffi/winapi
         "unsafe/private/win32.rkt")

;; Implements MysterX's "coclass" lookup, which is deprecated
(provide com-all-coclasses
         com-all-controls
         coclass->clsid
         clsid->coclass)

;; ----------------------------------------
;; Registry

(define _HKEY (_cpointer/null 'HKEY))

(define KEY_QUERY_VALUE #x1)
(define KEY_SET_VALUE   #x2)
(define KEY_READ #x20019)

(define ERROR_SUCCESS 0)
(define ERROR_MORE_DATA 234)
(define ERROR_NO_MORE_ITEMS 259)

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

(define-advapi RegOpenKeyExW (_hfun _HKEY _string/utf-16 _DWORD _REGSAM (hkey : (_ptr o _HKEY))
                                    -> RegOpenKeyExW hkey))

(define-advapi RegEnumKeyExW (_wfun _HKEY _DWORD _pointer (_ptr io _DWORD) 
                                    (_pointer = #f) ; reserved; must be NULL
                                    (_pointer = #f) (_pointer = #f) ; class
                                    (_pointer = #f) ; filetime
                                    -> (r : _LONG)))
(define (RegEnumKeyExW* hkey index)
  (let loop ([sz 256])
    (define bstr (make-bytes sz))
    (define r (RegEnumKeyExW hkey index bstr (quotient sz 2)))
    (cond
     [(= r ERROR_SUCCESS) (cast bstr _pointer _string/utf-16)]
     [(= r ERROR_MORE_DATA) (loop (* sz 2))]
     [(= r ERROR_NO_MORE_ITEMS) #f]
     [else (error "RegEnumKeyExW failed")])))

(define-advapi RegCreateKeyExW (_wfun _HKEY _string/utf-16 (_DWORD = 0) 
                                      (_pointer = #f) ; class
                                      _DWORD ; options
                                      _REGSAM
                                      _pointer ; security
                                      (hkey : (_ptr o _HKEY))
                                      (_ptr o _DWORD) ; disposition
                                      -> (r : _LONG)
                                      -> (and (= r ERROR_SUCCESS) hkey)))

(define-advapi RegQueryValueExW (_wfun _HKEY _string/utf-16 (_pointer = #f)
                                       (type : (_ptr o _DWORD))
                                       _pointer (len : (_ptr io _DWORD))
                                       -> (r : _LONG)
                                       -> (if (= r ERROR_SUCCESS) 
                                              (values len type)
                                              (values #f #f))))
(define-advapi RegSetValueExW (_wfun _HKEY _string/utf-16 (_pointer = #f)
                                     _DWORD _pointer _DWORD
                                     -> (r : _LONG)
                                     -> (= r ERROR_SUCCESS)))

(define-advapi RegCloseKey (_hfun _HKEY -> RegCloseKey (void)))

(define CLSIDLEN 38)

(define KEY_WOW64_64KEY #x0100)
(define KEY_WOW64_32KEY #x0200)

(define wow-flags
  (if win64?
      (list KEY_WOW64_64KEY KEY_WOW64_32KEY)
      (list 0)))

(define (enum-keys rx include-clsid? include-name? convert all?)
  (let wloop ([wow-flags wow-flags])
    (cond
     [(null? wow-flags) (if all? null #f)]
     [else
      (define r
        (let ([hkey (RegOpenKeyExW HKEY_CLASSES_ROOT  "CLSID" 0 
                                   (bitwise-ior (car wow-flags) KEY_READ))])
          (begin0
           (let loop ([key-index 0])
             (define sub (RegEnumKeyExW* hkey key-index))
             (cond
              [(not sub) (if all? null #f)]
              [(not (= CLSIDLEN (string-length sub)))
               ;; Bogus entry? Skip it.
               (loop (add1 key-index))]
              [(not (include-clsid? sub))
               (loop (add1 key-index))]
              [else
               (define sub-hkey (RegOpenKeyExW hkey sub 0 KEY_READ))
               (define buffer (make-bytes 256))
               (define-values (len type) (RegQueryValueExW sub-hkey "" buffer (bytes-length buffer)))
               (cond
                [(and type
                      (= type REG_SZ))
                 (define name (cast buffer _pointer _string/utf-16))
                 (if (include-name? name)
                     (let sloop ([sub-key-index 0])
                       (define subsub (RegEnumKeyExW* sub-hkey sub-key-index))
                       (cond
                        [(not subsub)
                         (RegCloseKey sub-hkey)
                         (loop (add1 key-index))]
                        [(regexp-match? rx subsub)
                         (RegCloseKey sub-hkey)
                         (define val (convert sub name subsub))
                         (if all?
                             (cons val (loop (add1 key-index)))
                             val)]
                        [else
                         (sloop (add1 sub-key-index))]))
                     (begin
                       (RegCloseKey sub-hkey)
                       (loop (add1 key-index))))]
                [else
                 (RegCloseKey sub-hkey)
                 (loop (add1 key-index))])]))
           (RegCloseKey hkey))))
      (cond
       [all? (append (wloop (cdr wow-flags)) r)]
       [r r]
       [else (wloop (cdr wow-flags))])])))

(define rx:object #rx"^(?i:InprocServer|InprocServer32|LocalServer|LocalServer32)$")
(define rx:control #rx"^(?i:control)$")

(define (com-all-coclasses)
  (sort-and-filter
   (enum-keys rx:object 
              (lambda (sub) #t)
              (lambda (name) #t)
              (lambda (sub name subsub) name) 
              #t)))

(define (com-all-controls)
  (sort-and-filter
   (enum-keys rx:control 
              (lambda (sub) #t)
              (lambda (name) #t)
              (lambda (sub name subsub) name) 
              #t)))

(define (sort-and-filter l)
  (let loop ([l (sort l string-ci<?)])
    (cond
     [(null? l) null]
     [(null? (cdr l)) l]
     [(string-ci=? (car l) (cadr l))
      (loop (cdr l))]
     [else (cons (car l) (loop (cdr l)))])))

(define (coclass->clsid coclass)
  (enum-keys rx:object 
             (lambda (sub) #t)
             (lambda (name) (equal? name coclass))
             (lambda (sub name subsub) (string->guid sub))
             #f))

(define (clsid->coclass clsid)
  (enum-keys rx:object 
             (lambda (sub)
               (define clsid2 (string->guid sub))
               (guid=? clsid clsid2))
             (lambda (name) #t)
             (lambda (sub name subsub) name)
             #f))
