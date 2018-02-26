#lang racket/base
(require "../port/string-output.rkt"
         "write-with-max.rkt")

(provide print-bytes)

(define (print-bytes bstr o max-length)
  (let ([max-length (write-bytes/max #"#\"" o max-length)])
    (define len (bytes-length bstr))
    (let loop ([start-i 0] [i 0] [max-length max-length])
      (cond
       [(eq? max-length 'full) 'full]
       [(or (= i len)
            (and max-length ((- i start-i) . > . max-length)))
        (let ([max-length (write-bytes/max bstr o max-length start-i i)])
          (write-bytes/max #"\"" o max-length))]
       [else
        (define b (bytes-ref bstr i))
        (cond
         [(and (b . < . 128)
               (let ([c (integer->char b)])
                 (and (or (char-blank? c)
                          (char-graphic? c))
                      (not (char=? c #\tab))
                      (not (char=? c #\"))
                      (not (char=? c #\\)))))
          (loop start-i (add1 i) max-length)]
         [else
          (let* ([max-length (write-bytes/max bstr o max-length start-i i)])
            (define escaped
              (case (and (b . < . 128) (integer->char b))
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
              (let ([max-length (write-bytes/max escaped o max-length)]
                    [i (add1 i)])
                (loop i i max-length))]
             [else
              (let ([i (add1 i)])
                (define next-b (or (and (i . < . len)
                                        (bytes-ref bstr i))
                                   0))
                (cond
                 [(or (b . >= . 64)
                      (and (>= next-b (char->integer #\0))
                           (<= next-b (char->integer #\7))))
                  (let* ([max-length (write-bytes/max #"\\" o max-length)]
                         [max-length (write-bytes/max (digit (arithmetic-shift b -6)) o max-length)]
                         [max-length (write-bytes/max (digit (bitwise-and 7 (arithmetic-shift b -3))) o max-length)]
                         [max-length (write-bytes/max (digit (bitwise-and 7 b)) o max-length)])
                    (loop i i max-length))]
                 [(b . >= . 8)
                  (let* ([max-length (write-bytes/max #"\\" o max-length)]
                         [max-length (write-bytes/max (digit (bitwise-and 7 (arithmetic-shift b -3))) o max-length)]
                         [max-length (write-bytes/max (digit (bitwise-and 7 b)) o max-length)])
                    (loop i i max-length))]
                 [else
                  (let* ([max-length (write-bytes/max #"\\" o max-length)]
                         [max-length (write-bytes/max (digit b) o max-length)])
                    (loop i i max-length))]))]))])]))))

(define (digit v)
  (case v
    [(0) #"0"]
    [(1) #"1"]
    [(2) #"2"]
    [(3) #"3"]
    [(4) #"4"]
    [(5) #"5"]
    [(6) #"6"]
    [(7) #"7"]))
