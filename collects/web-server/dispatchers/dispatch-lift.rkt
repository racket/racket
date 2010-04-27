#lang scheme/base
(require mzlib/contract)
(require "dispatch.ss"
         web-server/http
         web-server/http/response)
(provide/contract
 [interface-version dispatcher-interface-version/c]
 [make ((request? . -> . response/c) . -> . dispatcher/c)])

(define interface-version 'v1)
(define ((make procedure) conn req)
  (output-response/method
   conn
   (procedure req)
   (request-method req)))
