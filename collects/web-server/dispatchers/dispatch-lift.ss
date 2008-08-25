#lang scheme/base
(require mzlib/contract)
(require "dispatch.ss"
         "../private/response.ss"
         "../private/request-structs.ss"
         "../private/response-structs.ss")
(provide/contract
 [interface-version dispatcher-interface-version/c]
 [make ((request? . -> . response?) . -> . dispatcher/c)])

(define interface-version 'v1)
(define ((make procedure) conn req)
  (output-response/method
   conn
   (procedure req)
   (request-method req)))
