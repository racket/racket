#lang racket/base
(require "../port/string-output.rkt"
         "../port/bytes-output.rkt"
         "write-with-max.rkt")

(provide print-char)

(define (print-char c o max-length)
  (define esc-str
    (case c
      [(#\nul) "#\\nul"]
      [(#\backspace) "#\\backspace"]
      [(#\tab) "#\\tab"]
      [(#\page) "#\\page"]
      [(#\newline) "#\\newline"]
      [(#\return) "#\\return"]
      [(#\vtab) "#\\vtab"]
      [(#\space) "#\\space"]
      [(#\rubout) "#\\rubout"]
      [else #f]))
  (cond
   [esc-str
    (write-string/max esc-str o max-length)]
   [(char-graphic? c)
    (let ([max-length (write-string/max "#\\" o max-length)])
      (write-string/max (string c) o max-length))]
   [else
    (define n (char->integer c))
    (define (pad n s)
      (define len (string-length s))
      (if (len . <  . n)
          (string-append (make-string (- n len) #\0) s)
          s))
    (cond
     [(n . <= . #xFFFF)
      (let ([max-length (write-string/max "#\\u" o max-length)])
        (write-string/max (pad 4 (string-upcase (number->string n 16))) o max-length))]
     [else
      (let ([max-length (write-string/max "#\\U" o max-length)])
        (write-string/max (pad 8 (string-upcase (number->string n 16))) o max-length))])]))
