#lang racket/base
(require "config.rkt"
         "special.rkt"
         "wrap.rkt"
         "readtable.rkt"
         "consume.rkt"
         "accum-string.rkt"
         "error.rkt"
         "digit.rkt")

(provide read-character)

(define (read-character in config)
  (define c (read-char/special in config))
  (define char
    (cond
     [(eof-object? c)
      (reader-error in config #:due-to c
                    "expected a character after `#\\`")]
     [(not (char? c))
      (reader-error in config #:due-to c
                    "found non-character after `#\\`")]
     [(octal-digit? c)
      ;; Maybe octal
      (define c2 (peek-char/special in config))
      (cond
       [(and (char? c2) (octal-digit? c2))
        ;; Octal -- must be 3 digits
        (consume-char in c2)
        (define c3 (read-char/special in config))
        (define v
          (cond
           [(and (char? c3) (octal-digit? c3))
            (+ (arithmetic-shift (digit->number c) 6)
               (arithmetic-shift (digit->number c2) 3)
               (digit->number c3))]
           [else #f]))
        (unless (and v (v . <= . 255))
          (reader-error in config #:due-to c3
                        "bad character constant `#\\~a~a~a`"
                        c c2 (if (char? c3) c3 "")))
        (integer->char v)]
       [else
        ;; Not octal
        c])]
     [(or (char=? c #\u)
          (char=? c #\U))
      ;; Maybe hex encoding
      (define accum-str (accum-string-init! config))
      (define v (read-digits in config accum-str
                             #:base 16
                             #:max-count (if (char=? c #\u) 4 8)))
      (cond
       [(integer? v)
        ;; It's a hex encoding, but make sure it's in range
        (cond
         [(and (or (v . < . #xD800) (v . > . #xDFFF))
               (v . <= . #x10FFFF))
          (accum-string-abandon! accum-str config)
          (integer->char v)]
         [else
          (reader-error in config
                        "bad character constant `#\\u~a`"
                         (accum-string-get! accum-str config))])]
       [else
        ;; Not a hex encoding
        (accum-string-abandon! accum-str config)
        c])]
     [(char-alphabetic? c)
      ;; Maybe a name
      (define next-c (peek-char/special in config))
      (cond
       [(and (char? next-c)
             (char-alphabetic? next-c))
        ;; Must be a name
        (define accum-str (accum-string-init! config))
        (accum-string-add! accum-str c)
        (accum-string-add! accum-str next-c)
        (consume-char in next-c)
        (let loop ()
          (define next-c (peek-char/special in config))
          (when (and (char? next-c)
                     (char-alphabetic? next-c))
            (accum-string-add! accum-str next-c)
            (consume-char in next-c)
            (loop)))
        (define name (string-foldcase
                      (accum-string-get! accum-str config)))
        (case name
          [("nul" "null") #\nul]
          [("backspace") #\backspace]
          [("tab") #\tab]
          [("newline" "linefeed") #\newline]
          [("vtab") #\vtab]
          [("page") #\page]
          [("return") #\return]
          [("space") #\space]
          [("rubout") #\rubout]
          [else
           (reader-error in config
                         "bad character constant `#\\~a`"
                         name)])]
       [else
        ;; Not a name
        c])]
     [else
      ;; Any other character
      c]))

  (wrap char
        in
        config
        char))
