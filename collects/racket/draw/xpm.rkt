#lang racket/base

(provide read-xpm)

(define rx:start ; maybe skip comments here?
  #px#"^\\s*static\\s+char\\s*\\*\\s*[^][\\s]+\\s*\\[\\s*\\]\\s*=\\s*\\{")

(define rx:get-string ; skips spaces, comments, commas
  #rx#"^(?:[, \t\r\n]+|/\\*.*?\\*/)*\"([^\"\\]*)\"")

(define rx:color-spec
  ;; look for a simple `c' color, only `None' or `#hhhhhh', skip an `s' one
  #px#"^(.)\\s*(?:s\\s*[^ ]+\\s*)?c\\s*(?i:(none)|#([0-9a-f]{6}))")

(define (read-xpm in)
  (define (err why) (error 'read-xpm (format "~a: ~v" why in)))
  (define colors (make-hasheq)) ; byte -> RGBA as a 4-byte-string
  (define (get-string)
    (cond [(regexp-match rx:get-string in) => cadr]
          [else (err "insufficient strings")]))
  (define (bytes->int bs radix)
    (string->number (bytes->string/utf-8 bs) radix))
  (define (read-color)
    (let ([s (regexp-match rx:color-spec (get-string))]
          [b (make-bytes 4 0)])
      (unless (caddr s) ; matched "none"
        (let ([c (cadddr s)])
          (bytes-set! b 0 (bytes->int (subbytes c 0 2) 16))
          (bytes-set! b 1 (bytes->int (subbytes c 2 4) 16))
          (bytes-set! b 2 (bytes->int (subbytes c 4 6) 16))
          (bytes-set! b 3 #xFF)))
      (hash-set! colors (bytes-ref (cadr s) 0) b)))
  (define (read-meta)
    (define m
      (or (regexp-match
           #px"^\\s*([0-9]+)\\s*([0-9]+)\\s*([0-9]+)\\s*1(?:\\s|$)"
           (get-string))
          (err "unrecognized format")))
    (for ([i (in-range (bytes->int (cadddr m) 10))]) (read-color))
    (values (bytes->int (cadr m) 10) (bytes->int (caddr m) 10)))
  (unless (equal? "/* XPM */" (read-line in 'any)) (err "not an XPM file"))
  (unless (regexp-match? rx:start in) (err "expected C prefix not found"))
  (let*-values ([(width height) (read-meta)]
                [(result) (make-vector height)]
                [(buflen) (* width 4)])
    (for/list ([row (in-range height)])
      (let ([line (get-string)] [buf (make-bytes buflen)])
        (unless (= width (bytes-length line)) (err "malformed pixels data"))
        (for ([i (in-range width)])
          (bytes-copy! buf (* 4 i) (hash-ref colors (bytes-ref line i))))
        (vector-set! result row buf)))
    (values width height result)))
