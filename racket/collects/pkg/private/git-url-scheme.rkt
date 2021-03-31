#lang racket/base

(provide git-url-scheme?)

(define (git-url-scheme? s)
  (or (equal? s "git+http")
      (equal? s "git+https")))

