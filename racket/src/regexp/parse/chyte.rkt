#lang racket/base
(require (for-syntax racket/base))

;; A "chytes" is a string or byte string, and a "chyte"
;; is represented as an integer.

(provide chyte
         chytes-length
         chytes-ref
         chytes-ref/char
         chytes-limit)

(define-syntax (chyte stx)
  (syntax-case stx ()
    [(_ ch)
     (char? (syntax-e #'ch))
     #`(quote #,(char->integer (syntax-e #'ch)))]))

(define (chytes-length s)
  (if (bytes? s)
      (bytes-length s)
      (string-length s)))

(define (chytes-ref s i)
  (if (bytes? s)
      (bytes-ref s i)
      (char->integer (string-ref s i))))

(define (chytes-ref/char s i)
  (if (bytes? s)
      (integer->char (bytes-ref s i))
      (string-ref s i)))

(define (chytes-limit s)
  (if (bytes? s)
      255
      #x10FFFF))
