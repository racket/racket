#lang racket/base
(require (for-syntax racket/base)
         "../common/check.rkt")

(provide (all-from-out "../common/check.rkt")
         check-convention
         check-path-string
         check-path-bytes)

(define (check-convention who c)
  (check who (lambda (c) (or (eq? c 'windows) (eq? c 'unix)))
         #:contract "(or/c 'windows 'unix)"
         c))

(define (check-path-string who s)
  (when (zero? (string-length s))
    (raise-arguments-error who "path string is empty"))
  (for ([c (in-string s)])
    (when (char=? c #\nul)
      (raise-arguments-error who "path string contains a nul character"
                             "path string" s))))

(define (check-path-bytes who s)
  (when (zero? (bytes-length s))
    (raise-arguments-error who "byte string is empty"))
  (for ([c (in-bytes s)])
    (when (zero? c)
      (raise-arguments-error who "byte string contains a nul character"
                             "byte string" s))))
