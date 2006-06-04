(module dispatch-pathprocedure mzscheme
  (require "dispatch.ss"
           "../util.ss"
           "../response.ss")
  (provide interface-version
           gen-dispatcher)
  
  (define interface-version 'v1)
  (define ((gen-dispatcher the-path procedure) conn req)
    (let-values ([(uri method path) (decompose-request req)])
      (if (string=? the-path path)
          (output-response/method
           conn
           (procedure)
           method)
          (next-dispatcher)))))