#lang racket
(require web-server/templates)
(provide (all-defined-out))
(define interface-version 'v1)
(define timeout +inf.0)

(define (start initial-request)
  (response/template (include-template "static.html")))
