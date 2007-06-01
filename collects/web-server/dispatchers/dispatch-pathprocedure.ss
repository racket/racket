(module dispatch-pathprocedure mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net"))
  (require "dispatch.ss"
           "../private/util.ss"
           "../private/response.ss"
           "../private/request-structs.ss"
           "../private/response-structs.ss")
  (provide/contract
   [interface-version dispatcher-interface-version?]
   [make (string? (request? . -> . response?) . -> . dispatcher?)])
  
  (define interface-version 'v1)
  (define ((make the-path procedure) conn req)
    (define path (url-path->string (url-path (request-uri req))))
    (if (string=? the-path path)
        (output-response/method
         conn
         (procedure req)
         (request-method req))
        (next-dispatcher))))