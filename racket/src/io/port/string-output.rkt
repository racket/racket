#lang racket/base
(require "../common/check.rkt"
         "parameter.rkt"
         "output-port.rkt"
         "../string/convert.rkt"
         (submod "bytes-output.rkt" internal))

(provide write-char
         write-string)

(define/who (write-char ch [out (current-output-port)])
  (check who char? ch)
  (check who output-port? out)
  (write-string (string ch) out 0 1)
  (void))

(define/who (write-string str [out (current-output-port)] [start 0] [end (and (string? str)
                                                                              (string-length str))])
  (check who string? str)
  (check who output-port? out)
  (check who exact-nonnegative-integer? start)
  (check who exact-nonnegative-integer? end)
  (check-range who start end (string-length str) str)
  (let ([out (->core-output-port out)])
    (let loop ([i start])
      (cond
        [(= i end) (- i start)]
        [else
         (define next-i (min end (+ i 4096)))
         (define bstr (string->bytes/utf-8 str 0 i next-i))
         (do-write-bytes who out bstr 0 (bytes-length bstr))
         (loop next-i)]))))
