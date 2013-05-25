#lang racket/base
(require web-server/templates
         web-server/http
         xml)
(provide (all-defined-out))
(define interface-version 'v1)
(define timeout +inf.0)

(define (start initial-request)
  (response/xexpr
   `(html (pre ,(include-template "static.html"))
          "versus"
          ,(make-cdata #f #f (include-template "static.html")))))
