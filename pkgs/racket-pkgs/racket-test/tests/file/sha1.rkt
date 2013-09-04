#lang racket/base

(module+ test
  (require file/sha1 rackunit)
  ;; The docs say that sha1 must return a 40-character string,
  ;; and should include leading zeros.
  (check-equal? (string-length (sha1 (open-input-string ""))) 40)
  (check-equal? (string-length (sha1 (open-input-string " r a c k et"))) 40)

  (check-equal? (hex-string->bytes "") (bytes))
  (check-equal? (hex-string->bytes "00") (bytes 0))
  (check-equal? (hex-string->bytes "Af") (bytes 175))
  (define s "1234567890abcdef")
  (check-equal? (bytes->hex-string (hex-string->bytes s)) s)
  
  (check-exn exn:fail:contract? (lambda () (hex-string->bytes "1"))))
