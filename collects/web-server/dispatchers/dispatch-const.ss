(module dispatch-const mzscheme
  (require (lib "contract.ss"))
  (require "dispatch.ss"
           "../private/response.ss"
           "../request-structs.ss"
           "../response-structs.ss")
  (provide/contract
   [interface-version dispatcher-interface-version?]
   [make ((request? . -> . response?) . -> . dispatcher?)])
  
  (define interface-version 'v1)
  (define ((make procedure) conn req)
    (output-response/method
     conn
     (procedure req)
     (request-method req))))