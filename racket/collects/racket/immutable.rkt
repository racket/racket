#lang racket/base

;; This module provides operations that return immutable
;; strings, immutable byte-strings, or immutable vectors.

(provide immutable-string?
         immutable-bytes?
         immutable-vector?
         ;; String Operations
         make-immutable-string
         build-immutable-string
         immutable-string
         immutable-substring
         immutable-string-append
         immutable-string-append*
         immutable-string-join
         list->immutable-string
         number->immutable-string
         symbol->immutable-string
         keyword->immutable-string
         immutable-string-normalize-spaces
         immutable-string-replace
         immutable-string-split
         immutable-string-trim
         immutable-string-upcase
         immutable-string-downcase
         immutable-string-titlecase
         immutable-string-foldcase
         immutable-string-locale-upcase
         immutable-string-locale-downcase
         immutable-string-normalize-nfd
         immutable-string-normalize-nfkd
         immutable-string-normalize-nfc
         immutable-string-normalize-nfkc
         immutable-~a
         immutable-~v
         immutable-~s
         immutable-~e
         immutable-~r
         immutable-~.a
         immutable-~.v
         immutable-~.s
         ;; Byte String Operations
         make-immutable-bytes
         immutable-bytes
         immutable-subbytes
         immutable-bytes-append
         immutable-bytes-append*
         immutable-bytes-join
         list->immutable-bytes
         bytes->immutable-string/utf-8
         bytes->immutable-string/locale
         bytes->immutable-string/latin-1
         string->immutable-bytes/utf-8
         string->bytes/locale
         string->bytes/latin-1
         ;; Vector Operations
         make-immutable-vector
         list->immutable-vector
         build-immutable-vector
         immutable-vector-map
         immutable-vector-append
         immutable-vector-take
         immutable-vector-take-right
         immutable-vector-drop
         immutable-vector-drop-right
         immutable-vector-split-at
         immutable-vector-split-at-right
         immutable-vector-copy
         immutable-vector-filter
         immutable-vector-filter-not
         )

(require racket/bytes
         racket/format
         racket/string
         racket/vector
         (for-syntax racket/base))

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

;; Helper Macros

(define-syntax define-immutable-version
  (lambda (stx)
    (syntax-case stx ()
      [(define-immutable-version ->immutable
         [immutable-f f (arg ...)])
       #'(define-immutable-version ->immutable
           [immutable-f f (arg ...) () ()])]
      [(define-immutable-version ->immutable
         [immutable-f f (arg ...) ([opt-arg opt-arg-default] ...)])
       #'(define-immutable-version ->immutable
           [immutable-f f (arg ...) ([opt-arg opt-arg-default] ...) ()])]
      [(define-immutable-version ->immutable
         [immutable-f f
                      (arg ...)
                      ([opt-arg opt-arg-default] ...)
                      ([opt-kw [kw-arg kw-arg-default]] ...)])
       (with-syntax ([((kw/arg+def ...) ...) #'((opt-kw [kw-arg kw-arg-default]) ...)]
                     [((kw/arg ...) ...) #'((opt-kw kw-arg) ...)])
         #'(define (immutable-f arg ... [opt-arg opt-arg-default] ... kw/arg+def ... ...)
             (->immutable (f arg ... opt-arg ... kw/arg ... ...))))]
      [(define-immutable-version ->immutable
         [immutable-f f
                      (arg ...)
                      ([opt-arg opt-arg-default] ...)
                      ([opt-kw [kw-arg kw-arg-default]] ...)
                      rest-arg])
       (with-syntax ([((kw/arg+def ...) ...) #'((opt-kw [kw-arg kw-arg-default]) ...)]
                     [((kw/arg ...) ...) #'((opt-kw kw-arg) ...)])
         #'(define (immutable-f arg ... [opt-arg opt-arg-default] ... kw/arg+def ... ... . rest-arg)
             (->immutable (apply f arg ... opt-arg ... kw/arg ... ... rest-arg))))]
      )))

(define-syntax-rule
  (define-immutable-versions ->immutable stuff ...)
  (begin
    (define-immutable-version ->immutable stuff)
    ...))

;; Helper Conversion Functions

(define (maybe-string->immutable-string str)
  (and str (string->immutable-string str)))

(define (strings->immutable-strings strs)
  (map string->immutable-string strs))

;; String Operations

(define-immutable-versions string->immutable-string
  [make-immutable-string make-string (k) ([char #\nul])]
  [immutable-string string () () () chars]
  [immutable-string-append string-append () () () strs]
  [immutable-string-append* string-append* (str0) () () rest]
  [immutable-substring substring (str start) ([end (string-length str)])]
  [list->immutable-string list->string (lst)]
  [build-immutable-string build-string (n proc)]
  [symbol->immutable-string symbol->string (sym)]
  [keyword->immutable-string keyword->string (kw)]
  [bytes->immutable-string/utf-8 bytes->string/utf-8
                                 (bstr)
                                 ([err-char #f] [start 0] [end (bytes-length bstr)])]
  [bytes->immutable-string/locale bytes->string/locale
                                  (bstr)
                                  ([err-char #f] [start 0] [end (bytes-length bstr)])]
  [bytes->immutable-string/latin-1 bytes->string/latin-1
                                   (bstr)
                                   ([err-char #f] [start 0] [end (bytes-length bstr)])]
  [immutable-string-upcase string-upcase (str)]
  [immutable-string-downcase string-downcase (str)]
  [immutable-string-titlecase string-titlecase (str)]
  [immutable-string-foldcase string-foldcase (str)]
  [immutable-string-locale-upcase string-locale-upcase (str)]
  [immutable-string-locale-downcase string-locale-downcase (str)]
  [immutable-string-normalize-nfd string-normalize-nfd (str)]
  [immutable-string-normalize-nfkd string-normalize-nfkd (str)]
  [immutable-string-normalize-nfc string-normalize-nfc (str)]
  [immutable-string-normalize-nfkc string-normalize-nfkc (str)]
  [immutable-string-join string-join
                         (strs)
                         ([sep " "])
                         ([#:before-first [before-first ""]]
                          [#:before-last [before-last sep]]
                          [#:after-last [after-last ""]])]
  [immutable-string-normalize-spaces string-normalize-spaces
                                     (str)
                                     ([sep #px"\\s+"]
                                      [space " "])
                                     ([#:trim? [trim? #t]]
                                      [#:repeat? [repeat? #f]])]
  [immutable-string-replace string-replace
                            (str from to)
                            ()
                            ([#:all? [all? #t]])]
  [immutable-string-trim string-trim
                         (str)
                         ([sep #px"\\s+"])
                         ([#:left? [left? #t]]
                          [#:right? [right? #t]]
                          [#:repeat? [repeat? #f]])])

(define-immutable-version strings->immutable-strings
  [immutable-string-split string-split
                          (str)
                          ([sep #px"\\s+"])
                          ([#:trim? [trim? #t]]
                           [#:repeat? [repeat? #f]])])

(define-immutable-version maybe-string->immutable-string
  [number->immutable-string number->string (n) ([radix 10])])

;; Immutable versions of racket/format functions

(define-syntax-rule
  (define-immutable-versions/~a ->immutable
    [immutable-~a ~a separator-default limit-marker-default] ...)
  (define-immutable-versions ->immutable
    [immutable-~a ~a
                  ()
                  ()
                  ([#:separator [separator separator-default]]
                   [#:width [width #f]]
                   [#:max-width [max-width (or width +inf.0)]]
                   [#:min-width [min-width (or width 0)]]
                   [#:limit-marker [limit-marker limit-marker-default]]
                   [#:align [align 'left]]
                   [#:pad-string [pad-string " "]]
                   [#:left-pad-string [left-pad-string pad-string]]
                   [#:right-pad-string [right-pad-string pad-string]])
                  vs]
    ...))

(define-immutable-versions/~a string->immutable-string
  [immutable-~a ~a "" ""]
  [immutable-~v ~v " " "..."]
  [immutable-~s ~s " " "..."]
  [immutable-~e ~e " " "..."]
  [immutable-~.a ~.a "" ""]
  [immutable-~.v ~.v " " "..."]
  [immutable-~.s ~.s " " "..."]
  )

(define-immutable-version string->immutable-string
  [immutable-~r ~r
                (x)
                ()
                ([#:sign [sign #f]]
                 [#:base [base 10]]
                 [#:precision [precision 6]]
                 [#:notation [notation 'positional]]
                 [#:format-exponent [format-exponent #f]]
                 [#:min-width [min-width 1]]
                 [#:pad-string [pad-string " "]])])

;; Byte String Operations

(define-immutable-versions bytes->immutable-bytes
  [make-immutable-bytes make-bytes (k) ([b 0])]
  [immutable-bytes bytes () () () bs]
  [immutable-subbytes subbytes (bstr start) ([end (bytes-length bstr)])]
  [list->immutable-bytes list->bytes (lst)]
  [immutable-bytes-append bytes-append () () () bstrs]
  [immutable-bytes-append* bytes-append* (bstr0) () () rest]
  [immutable-bytes-join bytes-join (bstrs sep)]
  [string->immutable-bytes/utf-8 string->bytes/utf-8
                                 (str)
                                 ([err-byte #f] [start 0] [end (string-length str)])]
  [string->immutable-bytes/locale string->bytes/locale
                                  (str)
                                  ([err-byte #f] [start 0] [end (string-length str)])]
  [string->immutable-bytes/latin-1 string->bytes/latin-1
                                   (str)
                                   ([err-byte #f] [start 0] [end (string-length str)])])

;; Vector Operations

(define-immutable-versions vector->immutable-vector
  [make-immutable-vector make-vector (size) ([v 0])]
  [list->immutable-vector list->vector (lst)]
  [build-immutable-vector build-vector (n proc)]
  [immutable-vector-map vector-map (proc vec0) () () rest-vecs]
  [immutable-vector-append vector-append () () () vecs]
  [immutable-vector-take vector-take (vec pos)]
  [immutable-vector-take-right vector-take-right (vec pos)]
  [immutable-vector-drop vector-drop (vec pos)]
  [immutable-vector-drop-right vector-drop-right (vec pos)]
  [immutable-vector-copy vector-copy (vec) ([start 0] [end (vector-length vec)])]
  [immutable-vector-filter vector-filter (pred vec)]
  [immutable-vector-filter-not vector-filter-not (pred vec)])

(define (immutable-vector-split-at vec pos)
  (define-values [v1 v2]
    (vector-split-at vec pos))
  (values (vector->immutable-vector v1)
          (vector->immutable-vector v2)))

(define (immutable-vector-split-at-right vec pos)
  (define-values [v1 v2]
    (vector-split-at-right vec pos))
  (values (vector->immutable-vector v1)
          (vector->immutable-vector v2)))

