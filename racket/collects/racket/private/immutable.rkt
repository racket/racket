(module immutable "pre-base.rkt"

;; This module provides operations that return immutable
;; strings, immutable byte-strings, or immutable vectors.

(provide immutable-string?
         immutable-bytes?
         immutable-vector?
         ;; String Operations
         create-immutable-string?
         make-string
         string
         substring
         string-append
         list->string
         number->string
         symbol->string
         keyword->string
         string-upcase
         string-downcase
         string-titlecase
         string-foldcase
         string-locale-upcase
         string-locale-downcase
         string-normalize-nfd
         string-normalize-nfkd
         string-normalize-nfc
         string-normalize-nfkc
         ;; Byte String Operations
         create-immutable-bytes?
         make-bytes
         bytes
         subbytes
         bytes-append
         list->bytes
         bytes->string/utf-8
         bytes->string/locale
         bytes->string/latin-1
         string->bytes/utf-8
         string->bytes/locale
         string->bytes/latin-1
         ;; Vector Operations
         create-immutable-vector?
         make-vector
         list->vector
         )

(require (prefix-in - "pre-base.rkt")
         (for-syntax "pre-base.rkt"
                     "stxcase-scheme.rkt"
                     ))

;; Predicates

(define-syntax-rule (define-immutable-predicate immutable-v? v? immutable?)
  (define (immutable-v? v)
    (and (v? v) (immutable? v))))

(define-syntax-rule
  (define-immutable-predicates immutable? [immutable-v? v?] ...)
  (begin
    (define-immutable-predicate immutable-v? v? immutable?)
    ...))

(define-immutable-predicates immutable?
  [immutable-string? string?]
  [immutable-bytes? bytes?]
  [immutable-vector? vector?])

(define create-immutable-string? (make-parameter #false))
(define create-immutable-bytes? (make-parameter #false))
(define create-immutable-vector? (make-parameter #false))

;; Helper Macros

(define-syntax define-immutable-version
  (lambda (stx)
    (syntax-case stx ()
      [(define-immutable-version ->immutable create-immutable?
         [immutable-f f (arg ...)])
       #'(define-immutable-version ->immutable create-immutable?
           [immutable-f f (arg ...) () ()])]
      [(define-immutable-version ->immutable create-immutable?
         [immutable-f f (arg ...) ([opt-arg opt-arg-default] ...)])
       #'(define-immutable-version ->immutable create-immutable?
           [immutable-f f (arg ...) ([opt-arg opt-arg-default] ...) ()])]
      [(define-immutable-version ->immutable create-immutable?
         [immutable-f f
                      (arg ...)
                      ([opt-arg opt-arg-default] ...)
                      ([opt-kw [kw-arg kw-arg-default]] ...)])
       (with-syntax ([((kw/arg+def ...) ...) #'((opt-kw [kw-arg kw-arg-default]) ...)]
                     [((kw/arg ...) ...) #'((opt-kw kw-arg) ...)])
         #'(define (immutable-f arg ... [opt-arg opt-arg-default] ... kw/arg+def ... ...
                                #:immutable? [immutable? (create-immutable?)])
             (if immutable?
                 (->immutable (f arg ... opt-arg ... kw/arg ... ...))
                 (f arg ... opt-arg ... kw/arg ... ...))))]
      [(define-immutable-version ->immutable create-immutable?
         [immutable-f f
                      (arg ...)
                      ([opt-arg opt-arg-default] ...)
                      ([opt-kw [kw-arg kw-arg-default]] ...)
                      rest-arg])
       (with-syntax ([((kw/arg+def ...) ...) #'((opt-kw [kw-arg kw-arg-default]) ...)]
                     [((kw/arg ...) ...) #'((opt-kw kw-arg) ...)])
         #'(define (immutable-f arg ... [opt-arg opt-arg-default] ... kw/arg+def ... ...
                                #:immutable? [immutable? (create-immutable?)] . rest-arg)
             (if immutable?
                 (->immutable (apply f arg ... opt-arg ... kw/arg ... ... rest-arg))
                 (apply f arg ... opt-arg ... kw/arg ... ... rest-arg))))]
      )))

(define-syntax-rule
  (define-immutable-versions ->immutable create-immutable? stuff ...)
  (begin
    (define-immutable-version ->immutable create-immutable? stuff)
    ...))

;; Helper Conversion Functions

(define (maybe-string->immutable-string str)
  (and str (string->immutable-string str)))

(define (strings->immutable-strings strs)
  (map string->immutable-string strs))

;; String Operations

(define-immutable-versions string->immutable-string create-immutable-string?
  [make-string -make-string (k) ([char #\nul])]
  [string -string () () () chars]
  [string-append -string-append () () () strs]
  [substring -substring (str start) ([end (string-length str)])]
  [list->string -list->string (lst)]
  [symbol->string -symbol->string (sym)]
  [keyword->string -keyword->string (kw)]
  [bytes->string/utf-8 -bytes->string/utf-8
                       (bstr)
                       ([err-char #f] [start 0] [end (bytes-length bstr)])]
  [bytes->string/locale -bytes->string/locale
                        (bstr)
                        ([err-char #f] [start 0] [end (bytes-length bstr)])]
  [bytes->string/latin-1 -bytes->string/latin-1
                         (bstr)
                         ([err-char #f] [start 0] [end (bytes-length bstr)])]
  [string-upcase -string-upcase (str)]
  [string-downcase -string-downcase (str)]
  [string-titlecase -string-titlecase (str)]
  [string-foldcase -string-foldcase (str)]
  [string-locale-upcase -string-locale-upcase (str)]
  [string-locale-downcase -string-locale-downcase (str)]
  [string-normalize-nfd -string-normalize-nfd (str)]
  [string-normalize-nfkd -string-normalize-nfkd (str)]
  [string-normalize-nfc -string-normalize-nfc (str)]
  [string-normalize-nfkc -string-normalize-nfkc (str)])

(define-immutable-version maybe-string->immutable-string create-immutable-string?
  [number->string -number->string (n) ([radix 10])])

;; Byte String Operations

(define-immutable-versions bytes->immutable-bytes create-immutable-bytes?
  [make-bytes -make-bytes (k) ([b 0])]
  [bytes -bytes () () () bs]
  [subbytes -subbytes (bstr start) ([end (bytes-length bstr)])]
  [list->bytes -list->bytes (lst)]
  [bytes-append -bytes-append () () () bstrs]
  [string->bytes/utf-8 -string->bytes/utf-8
                       (str)
                       ([err-byte #f] [start 0] [end (string-length str)])]
  [string->bytes/locale -string->bytes/locale
                        (str)
                        ([err-byte #f] [start 0] [end (string-length str)])]
  [string->bytes/latin-1 -string->bytes/latin-1
                         (str)
                         ([err-byte #f] [start 0] [end (string-length str)])])

;; Vector Operations

(define-immutable-versions vector->immutable-vector create-immutable-vector?
  [make-vector -make-vector (size) ([v 0])]
  [list->vector -list->vector (lst)])

)
