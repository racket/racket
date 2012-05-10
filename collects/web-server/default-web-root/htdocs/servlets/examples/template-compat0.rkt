#lang racket/base
(require web-server/templates
         web-server/http
         web-server/compat/0/coerce)
(provide (all-defined-out))
(define interface-version 'v1)
(define timeout +inf.0)

(define (start initial-request)
  (cons TEXT/HTML-MIME-TYPE
        (list (string->bytes/utf-8 (include-template "static.html")))))
