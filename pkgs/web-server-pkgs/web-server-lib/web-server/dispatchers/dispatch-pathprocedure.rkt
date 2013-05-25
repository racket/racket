#lang racket/base
(require mzlib/contract
         net/url)
(require web-server/dispatchers/dispatch
         web-server/private/util
         web-server/http
         web-server/http/response)
(provide/contract
 [interface-version dispatcher-interface-version/c]
 [make (string? (request? . -> . response?) . -> . dispatcher/c)])

(define interface-version 'v1)
(define ((make the-path procedure) conn req)
  (define path (url-path->string (url-path (request-uri req))))
  (if (string=? the-path path)
      (output-response/method
       conn
       (procedure req)
       (request-method req))
      (next-dispatcher)))
