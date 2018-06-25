#lang racket/base
(provide sha1
         sha1-bytes
         bytes->hex-string
         hex-string->bytes)

(define (sha1 in [start 0] [end #f])
  (bytes->hex-string (sha1-bytes in start end)))

(define (bytes->hex-string bstr)
  (let* ([len (bytes-length bstr)]
         [bstr2 (make-bytes (* len 2))]
         [digit
          (lambda (v)
            (if (v . < . 10)
                (+ v (char->integer #\0))
                (+ v (- (char->integer #\a) 10))))])
    (for ([i (in-range len)])
      (let ([c (bytes-ref bstr i)])
        (bytes-set! bstr2 (* 2 i) (digit (arithmetic-shift c -4)))
        (bytes-set! bstr2 (+ (* 2 i) 1) (digit (bitwise-and c #xF)))))
    (bytes->string/latin-1 bstr2)))

(define (hex-string->bytes s)
  (unless (and (string? s) (regexp-match? #px"^([[:xdigit:]]{2})*$" s))
    (raise-argument-error 'hex-string->bytes
                          "(and/c string? #px\"^([[:xdigit:]]{2})*$\")" s))
  
  (define (hex-char->int c)
    (cond ((char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0)))
          ((char<=? #\a c #\f) (+ 10 (- (char->integer c) (char->integer #\a))))
          ((char<=? #\A c #\F) (+ 10 (- (char->integer c) (char->integer #\A))))))
  
  (define bsize (/ (string-length s) 2))
  (define b (make-bytes bsize))
  
  (for ((i (in-range bsize)))
    (define high (hex-char->int (string-ref s (+ i i))))
    (define low  (hex-char->int (string-ref s (+ i i 1))))
    (bytes-set! b i (+ (arithmetic-shift high 4) low)))
  
  b)

