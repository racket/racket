(module dispatch-pathprocedure mzscheme
  (require "dispatch.ss"
           "../util.ss"
           "../response.ss")
  (provide interface-version
           make)
  
  (define interface-version 'v1)
  (define ((make the-path procedure) conn req)
    (let-values ([(uri method path) (decompose-request req)])
      (if (string=? the-path path)
          (output-response/method
           conn
           (procedure)
           method)
          (next-dispatcher)))))