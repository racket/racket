#lang racket/base
(require net/url-string)

(provide github-url?)

(define (github-url? u)
  (or
   (equal? (url-scheme u) "github")
   (and (equal? (url-scheme u) "git")
        (equal? (url-host u) "github.com"))))
