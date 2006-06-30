(module dispatch-pathprocedure mzscheme
  (require (lib "contract.ss"))
  (require "dispatch.ss"
           "../util.ss"
           "../response.ss")
  (provide/contract
   [interface-version dispatcher-interface-version?]
   [make (string? (-> response?) . -> . dispatcher?)])
  
  (define interface-version 'v1)
  (define ((make the-path procedure) conn req)
    (let-values ([(uri method path) (decompose-request req)])
      (if (string=? the-path path)
          (output-response/method
           conn
           (procedure)
           method)
          (next-dispatcher)))))