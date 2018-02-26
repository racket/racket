#lang racket/base

(provide ucs-4-encoding
         string->bytes/ucs-4)

(define ucs-4-encoding
  (if (system-big-endian?)
      "UCS-4BE"
      "UCS-4LE"))

(define (string->bytes/ucs-4 str start end)
  (define len (* 4 (- end start)))
  (define bstr (make-bytes len))
  (if (system-big-endian?)
      (for ([c (in-string str start end)]
            [i (in-range 0 len 4)])
        (define n (char->integer c))
        (bytes-set! bstr i (arithmetic-shift n -24))
        (bytes-set! bstr (+ i 1) (bitwise-and 255 (arithmetic-shift n -16)))
        (bytes-set! bstr (+ i 2) (bitwise-and 255 (arithmetic-shift n -8)))
        (bytes-set! bstr (+ i 3) (bitwise-and 255 n)))
      (for ([c (in-string str start end)]
            [i (in-range 0 len 4)])
        (define n (char->integer c))
        (bytes-set! bstr (+ i 3) (arithmetic-shift n -24))
        (bytes-set! bstr (+ i 2) (bitwise-and 255 (arithmetic-shift n -16)))
        (bytes-set! bstr (+ i 1) (bitwise-and 255 (arithmetic-shift n -8)))
        (bytes-set! bstr i (bitwise-and 255 n))))
  bstr)
