(module dispatch-host mzscheme
  (require "dispatch.ss"
           "../servlet-helpers.ss")
  (provide interface-version
           make)
  
  (define interface-version 'v1)
  (define (make lookup-dispatcher)
    (lambda (conn req)
      (let* ([host (get-host (request-uri req) (request-headers/raw req))])
        ((lookup-dispatcher host) conn req)))))