#lang racket/base

(require
 racket/format
 racket/list
 "template.rkt"
) ; end require

(provide
 render-template
 render-fstring
 format-fstring-value
 template->sql
 html-render
) ; end provide

(define (checked-template-parts who tpl)
  (unless (template? tpl)
    (raise-argument-error who "template?" tpl)
  ) ; end unless template?
  (define strings (template-strings tpl))
  (define interpolations (template-interpolations tpl))
  (unless (and (list? strings)
               (andmap string? strings)
          ) ; end and
    (raise-argument-error who "(listof string?)" strings)
  ) ; end unless strings
  (unless (and (list? interpolations)
               (andmap interpolation? interpolations)
          ) ; end and
    (raise-argument-error who "(listof interpolation?)" interpolations)
  ) ; end unless interpolations
  (unless (= (length strings)
             (add1 (length interpolations))
          ) ; end =
    (raise-arguments-error who
                           "template strings and interpolations have incompatible lengths"
                           "strings"
                           strings
                           "interpolations"
                           interpolations
    ) ; end raise-arguments-error
  ) ; end unless length invariant
  (values strings interpolations)
) ; end define checked-template-parts

(define (render-template tpl
                         #:interpolation->string (interpolation->string interpolation-value)
                         #:value->string (value->string ~a)
        ) ; end define formals
  (define-values (strings interpolations)
    (checked-template-parts 'render-template tpl)
  ) ; end define-values
  (define out (open-output-string))
  (let loop ((strings strings)
             (interpolations interpolations)
        ) ; end loop bindings
    (write-string (car strings) out)
    (cond
      ((null? interpolations)
       (void)
      ) ; end no more interpolations
      (else
       (write-string (value->string (interpolation->string (car interpolations))) out)
       (loop (cdr strings)
             (cdr interpolations)
       ) ; end loop
      ) ; end interpolation part
    ) ; end cond
  ) ; end let loop
  (get-output-string out)
) ; end define render-template

(define (render-fstring tpl)
  (render-template tpl)
) ; end define render-fstring

(define (format-fstring-value value format-spec conversion)
  (define converted-value (apply-conversion value conversion))
  (cond
    ((not format-spec)
     (~a converted-value)
    ) ; end no format spec
    ((converted-by-string-conversion? conversion)
     (format-string-value converted-value format-spec)
    ) ; end string conversion
    (else
     (format-value converted-value format-spec)
    ) ; end ordinary value
  ) ; end cond
) ; end define format-fstring-value

(define (apply-conversion value conversion)
  (cond
    ((equal? conversion "") value)
    ((equal? conversion "s") (~a value))
    ((equal? conversion "r") (~v value))
    ((equal? conversion "a") (~v value))
    (else
     (raise-arguments-error 'format-fstring-value
                            "unsupported conversion"
                            "conversion"
                            conversion
     ) ; end raise-arguments-error
    ) ; end unsupported conversion
  ) ; end cond
) ; end define apply-conversion

(define (converted-by-string-conversion? conversion)
  (not (equal? conversion ""))
) ; end define converted-by-string-conversion?

(struct format-options (fill align sign zero? width precision type) #:transparent)

(define (format-value value spec)
  (define options (parse-format-spec spec))
  (define type (format-options-type options))
  (cond
    ((memv type '(#f #\s))
     (format-string-value (~a value) spec)
    ) ; end string-like format
    ((memv type '(#\d #\b #\o #\x #\X))
     (format-integer-value value options)
    ) ; end integer format
    ((memv type '(#\f #\F #\e #\E #\g #\G #\%))
     (format-real-value value options)
    ) ; end real format
    (else
     (raise-arguments-error 'format-fstring-value
                            "unsupported format type"
                            "format-spec"
                            spec
     ) ; end raise-arguments-error
    ) ; end unsupported type
  ) ; end cond
) ; end define format-value

(define (format-string-value value spec)
  (define options (parse-format-spec spec))
  (define type (format-options-type options))
  (unless (or (not type) (char=? type #\s))
    (raise-arguments-error 'format-fstring-value
                           "numeric format type cannot be used after string conversion"
                           "format-spec"
                           spec
    ) ; end raise-arguments-error
  ) ; end unless string type
  (define text (if (format-options-precision options)
                   (substring value
                              0
                              (min (string-length value)
                                   (format-options-precision options)
                              ) ; end min
                   ) ; end substring
                   value
               ) ; end if
  ) ; end define text
  (apply-alignment text options #f)
) ; end define format-string-value

(define (format-integer-value value options)
  (unless (integer? value)
    (raise-argument-error 'format-fstring-value "integer?" value)
  ) ; end unless integer
  (define type (format-options-type options))
  (define base
    (case type
      ((#\b) 2)
      ((#\o) 8)
      ((#\x #\X) 16)
      (else 10)
    ) ; end case
  ) ; end define base
  (define is-negative? (negative? value))
  (define digits (number->string (abs value) base))
  (define cased-digits
    (if (eqv? type #\X)
        (string-upcase digits)
        digits
    ) ; end if
  ) ; end define cased-digits
  (apply-alignment (string-append (sign-prefix is-negative? (format-options-sign options))
                                  cased-digits
                   ) ; end string-append
                   options
                   #t
  ) ; end apply-alignment
) ; end define format-integer-value

(define (format-real-value value options)
  (unless (real? value)
    (raise-argument-error 'format-fstring-value "real?" value)
  ) ; end unless real
  (define type (format-options-type options))
  (define percent? (eqv? type #\%))
  (define base-value (if percent? (* value 100) value))
  (define precision
    (or (format-options-precision options)
        (if (memv type '(#\f #\F #\e #\E #\%)) 6 #f)
    ) ; end or
  ) ; end define precision
  (define notation
    (if (memv type '(#\e #\E))
        'exponential
        'positional
    ) ; end if
  ) ; end define notation
  (define raw
    (if precision
        (~r base-value #:notation notation #:precision (list '= precision))
        (~r base-value #:notation notation)
    ) ; end if
  ) ; end define raw
  (define exponent-cased
    (if (memv type '(#\E #\G))
        (string-upcase raw)
        raw
    ) ; end if
  ) ; end define exponent-cased
  (define with-percent
    (if percent?
        (string-append exponent-cased "%")
        exponent-cased
    ) ; end if
  ) ; end define with-percent
  (define is-negative? (and (real? base-value) (negative? base-value)))
  (define unsigned-text
    (if is-negative?
        (substring with-percent 1)
        with-percent
    ) ; end if
  ) ; end define unsigned-text
  (apply-alignment (string-append (sign-prefix is-negative? (format-options-sign options))
                                  unsigned-text
                   ) ; end string-append
                   options
                   #t
  ) ; end apply-alignment
) ; end define format-real-value

(define (sign-prefix negative? sign)
  (cond
    (negative? "-")
    ((eqv? sign #\+) "+")
    ((eqv? sign #\space) " ")
    (else "")
  ) ; end cond
) ; end define sign-prefix

(define (parse-format-spec spec)
  (unless (string? spec)
    (raise-argument-error 'format-fstring-value "string?" spec)
  ) ; end unless string
  (define length (string-length spec))
  (define index 0)
  (define fill #\space)
  (define align #f)
  (when (and (< 1 length)
             (alignment-char? (string-ref spec 1))
        ) ; end and
    (set! fill (string-ref spec 0))
    (set! align (string-ref spec 1))
    (set! index 2)
  ) ; end when fill align
  (when (and (not align)
             (< index length)
             (alignment-char? (string-ref spec index))
        ) ; end and
    (set! align (string-ref spec index))
    (set! index (add1 index))
  ) ; end when align
  (define sign #f)
  (when (and (< index length)
             (sign-char? (string-ref spec index))
        ) ; end and
    (set! sign (string-ref spec index))
    (set! index (add1 index))
  ) ; end when sign
  (define zero? #f)
  (when (and (< index length)
             (char=? (string-ref spec index) #\0)
        ) ; end and
    (set! zero? #t)
    (set! index (add1 index))
  ) ; end when zero
  (define-values (width next-index) (read-digits spec index))
  (set! index next-index)
  (define precision #f)
  (when (and (< index length)
             (char=? (string-ref spec index) #\.)
        ) ; end and
    (define-values (parsed-precision after-precision)
      (read-digits spec (add1 index))
    ) ; end define-values
    (unless parsed-precision
      (raise-arguments-error 'format-fstring-value
                             "expected precision after ."
                             "format-spec"
                             spec
      ) ; end raise-arguments-error
    ) ; end unless precision
    (set! precision parsed-precision)
    (set! index after-precision)
  ) ; end when precision
  (define type #f)
  (when (< index length)
    (set! type (string-ref spec index))
    (set! index (add1 index))
  ) ; end when type
  (unless (= index length)
    (raise-arguments-error 'format-fstring-value
                           "could not parse format spec"
                           "format-spec"
                           spec
    ) ; end raise-arguments-error
  ) ; end unless fully parsed
  (format-options fill align sign zero? width precision type)
) ; end define parse-format-spec

(define (read-digits spec index)
  (define length (string-length spec))
  (let loop ((index index)
             (digits '())
        ) ; end loop bindings
    (cond
      ((and (< index length)
            (char-numeric? (string-ref spec index))
       ) ; end and
       (loop (add1 index)
             (cons (string-ref spec index) digits)
       ) ; end loop
      ) ; end digit
      ((null? digits)
       (values #f index)
      ) ; end no digits
      (else
       (values (string->number (list->string (reverse digits)))
               index
       ) ; end values
      ) ; end done
    ) ; end cond
  ) ; end let loop
) ; end define read-digits

(define (alignment-char? ch)
  (memv ch '(#\< #\> #\^ #\=))
) ; end define alignment-char?

(define (sign-char? ch)
  (memv ch '(#\+ #\- #\space))
) ; end define sign-char?

(define (apply-alignment text options numeric?)
  (define width (format-options-width options))
  (cond
    ((or (not width) (<= width (string-length text))) text)
    (else
     (define fill-string
       (string (if (and numeric?
                        (format-options-zero? options)
                        (not (format-options-align options))
                   ) ; end and
                   #\0
                   (format-options-fill options)
               ) ; end if
       ) ; end string
     ) ; end define fill-string
     (define align
       (or (format-options-align options)
           (if numeric? #\> #\<)
       ) ; end or
     ) ; end define align
     (define pad-count (- width (string-length text)))
     (cond
       ((and numeric?
             (or (eqv? align #\=)
                 (and (format-options-zero? options)
                      (not (format-options-align options))
                 ) ; end and
             ) ; end or
             (has-sign-prefix? text)
        ) ; end and
        (string-append (substring text 0 1)
                       (make-padding fill-string pad-count)
                       (substring text 1)
        ) ; end string-append
       ) ; end sign-aware padding
       ((eqv? align #\<)
        (string-append text (make-padding fill-string pad-count))
       ) ; end left align
       ((eqv? align #\^)
        (define left-count (quotient pad-count 2))
        (define right-count (- pad-count left-count))
        (string-append (make-padding fill-string left-count)
                       text
                       (make-padding fill-string right-count)
        ) ; end string-append
       ) ; end center align
       (else
        (string-append (make-padding fill-string pad-count) text)
       ) ; end right align
     ) ; end cond
    ) ; end needs padding
  ) ; end cond
) ; end define apply-alignment

(define (has-sign-prefix? text)
  (and (positive? (string-length text))
       (memv (string-ref text 0) '(#\+ #\- #\space))
  ) ; end and
) ; end define has-sign-prefix?

(define (make-padding fill-string count)
  (apply string-append (make-list count fill-string))
) ; end define make-padding

(define (template->sql tpl)
  (define-values (strings interpolations)
    (checked-template-parts 'template->sql tpl)
  ) ; end define-values
  (define out (open-output-string))
  (define params
    (let loop ((strings strings)
               (interpolations interpolations)
          ) ; end loop bindings
      (write-string (car strings) out)
      (cond
        ((null? interpolations)
         '()
        ) ; end no more interpolations
        (else
         (define part (car interpolations))
         (unless (identifier? (interpolation-syntax part))
           (raise-arguments-error 'template->sql
                                  "SQL template only accepts identifier slots"
                                  "interpolation"
                                  part
           ) ; end raise-arguments-error
         ) ; end unless identifier
         (write-string "?" out)
         (cons (interpolation-value part)
               (loop (cdr strings)
                     (cdr interpolations)
               ) ; end loop
         ) ; end cons
        ) ; end interpolation part
      ) ; end cond
    ) ; end let loop
  ) ; end define params
  (values (get-output-string out)
          params
  ) ; end values
) ; end define template->sql

(define (html-render tpl #:interpolation->string (interpolation->string interpolation-value))
  (render-template tpl
                   #:interpolation->string interpolation->string
                   #:value->string html-escape
  ) ; end render-template
) ; end define html-render

(define (html-escape value)
  (define out (open-output-string))
  (for ((ch (in-string (~a value))))
    (write-string
     (case ch
       ((#\<) "&lt;")
       ((#\>) "&gt;")
       ((#\&) "&amp;")
       ((#\") "&quot;")
       (else (string ch))
     ) ; end case
     out
    ) ; end write-string
  ) ; end for
  (get-output-string out)
) ; end define html-escape
