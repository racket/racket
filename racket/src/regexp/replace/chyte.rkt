#lang racket/base

(provide chytes-ref
         subchytes
         chytes-append
         chytes?
         chytes-length)

(define (chytes-ref s pos)
  (if (bytes? s)
      (bytes-ref s pos)
      (char->integer (string-ref s pos))))

(define (subchytes s a [b #f])
  (if (bytes? s)
      (subbytes s a (or b (bytes-length s)))
      (substring s a (or b (string-length s)))))

(define chytes-append
  (case-lambda
    [(a) a]
    [(a b) (if (bytes? a)
               (bytes-append a b)
               (string-append a b))]
    [(a b c) (if (bytes? a)
                 (bytes-append a b c)
                 (string-append a b c))]
    [(a . l) (if (bytes? a)
                 (apply bytes-append a l)
                 (apply string-append a l))]))

(define (chytes? ex v)
  (if (bytes? ex)
      (bytes? v)
      (string? v)))

(define (chytes-length s)
  (if (bytes? s)
      (bytes-length s)
      (string-length s)))

  
