#lang racket/base
(require "../port/string-output.rkt"
         "write-with-max.rkt")

(provide print-string)

(define (print-string str o max-length)
  (let ([max-length (write-bytes/max #"\"" o max-length)])
    (define len (string-length str))
    (let loop ([start-i 0] [i 0] [max-length max-length])
      (cond
       [(eq? max-length 'full) 'full]
       [(or (= i len)
            (and max-length ((- i start-i) . > . max-length)))
        (let ([max-length (write-string/max str o max-length start-i i)])
          (write-bytes/max #"\"" o max-length))]
       [else
        (define c (string-ref str i))
        (define escaped
          (case c
            [(#\") #"\\\""]
            [(#\\) #"\\\\"]
            [(#\u7) #"\\a"]
            [(#\backspace) #"\\b"]
            [(#\u1B) #"\\e"]
            [(#\page) #"\\f"]
            [(#\newline) #"\\n"]
            [(#\return) #"\\r"]
            [(#\tab) #"\\t"]
            [(#\vtab) #"\\v"]
            [else #f]))
        (cond
         [escaped
          (let* ([max-length (write-string/max str o max-length start-i i)]
                 [max-length (write-bytes/max escaped o max-length)]
                 [i (add1 i)])
            (loop i i max-length))]
         [(or (char-graphic? c)
              (char-blank? c))
          (loop start-i (add1 i) max-length)]
         [else
          (define n (char->integer c))
          (define (pad n s)
            (define len (string-length s))
            (if (len . <  . n)
                (string-append (make-string (- n len) #\0) s)
                s))
          (let* ([max-length (write-string/max str o max-length start-i i)]
                 [max-length
                  (cond
                   [(n . <= . #xFFFF)
                    (let ([max-length (write-bytes/max #"\\u" o max-length)])
                      (write-string/max (pad 4 (string-upcase (number->string n 16))) o max-length))]
                   [else
                    (let ([max-length (write-bytes/max #"\\U" o max-length)])
                      (write-string/max (pad 8 (string-upcase (number->string n 16))) o max-length))])]
                 [i (add1 i)])
            (loop i i max-length))])]))))
