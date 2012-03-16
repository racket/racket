#lang racket
(define-syntax-rule (reqpro m ...)
  (begin (require m ...)
         (provide (all-from-out m ...))))
(provide (all-from-out racket))
(reqpro web-server/lang/base
        net/url
        web-server/http
        web-server/http/bindings
        web-server/dispatch
        web-server/stuffers
        web-server/lang/abort-resume
        web-server/lang/web
        web-server/lang/native
        web-server/lang/web-cells
        web-server/lang/web-param
        web-server/lang/file-box
        web-server/lang/soft)

