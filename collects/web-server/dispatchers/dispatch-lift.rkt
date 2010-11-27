#lang racket/base
(require racket/contract)
(require web-server/dispatchers/dispatch
         web-server/http
         web-server/http/response)
(provide/contract
 [interface-version dispatcher-interface-version/c]
 [make ((request? . -> . response?) . -> . dispatcher/c)])

(define interface-version 'v1)
(define ((make procedure) conn req)
  (output-response/method
   conn
   (procedure req)
   (request-method req)))
