#lang racket/base
(require racket/cmdline file/gzip file/gunzip net/base64)

(define do-lang? #f)

(define (encode/decode-text who lang-from lang-to convert1 convert2)
  (when do-lang?
    (let ([l (cadr (or (regexp-match #rx"^ *#lang +(.*[^ ]) *$" (read-line))
                       (error who "missing #lang line")))])
      (if (equal? l lang-from)
        (printf "#lang ~a\n" lang-to)
        (error who "bad #lang: expected ~s, got ~s" lang-from l))))
  (define O (open-output-bytes))
  (convert1 (current-input-port) O)
  (convert2 (open-input-bytes (get-output-bytes O)) (current-output-port))
  (flush-output))

(define (encode-text)
  (encode/decode-text
   'encode-text "racket/base" "s-exp framework/private/decode"
   deflate base64-encode-stream))

(define (decode-text)
  (encode/decode-text
   'decode-text "s-exp framework/private/decode" "racket/base"
   base64-decode-stream inflate))

(command-line
 #:once-each
 ["-l" "translate lang line" (set! do-lang? #t)]
 #:once-any
 ["-e" "encode" (encode-text) (exit)]
 ["-d" "decode" (decode-text) (exit)])
(printf "Use `-h' for help\n")
