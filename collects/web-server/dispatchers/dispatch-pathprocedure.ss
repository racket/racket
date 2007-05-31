(module dispatch-pathprocedure mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net"))
  (require "dispatch.ss"
           "../private/util.ss"
           "../private/response.ss"
           "../request-structs.ss"
           "../response-structs.ss")
  (provide/contract
   [interface-version dispatcher-interface-version?]
   [make (string? (-> response?) . -> . dispatcher?)])
  
  (define interface-version 'v1)
  (define ((make the-path procedure) conn req)
    (define path (url-path->string (url-path (request-uri req))))
    (if (string=? the-path path)
        (output-response/method
         conn
         (procedure)
         (request-method req))
        (next-dispatcher))))