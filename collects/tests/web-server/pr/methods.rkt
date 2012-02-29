#lang racket

(require web-server/servlet-env
         web-server/http/xexpr
         web-server/http/request-structs
         web-server/dispatch)

(define-values (app-dispatch app-url)
    (dispatch-rules
     [("") index]
     [else index]))

(define (index req)
  (response/xexpr `(html (p ,(bytes->string/utf-8 (request-method req)))
                         (p ,(cond ((bytes? (request-post-data/raw req))
                                    (bytes->string/utf-8 (request-post-data/raw req)))
                                   (else "No request body"))))))

(serve/servlet app-dispatch
               #:port 8000
               #:servlet-regexp #rx""
               #:command-line? #t)
