(module dispatch-host mzscheme
  (require "dispatch.ss"
           "servlet-helpers.ss")
  (provide interface-version
           gen-dispatcher)
  
  (define interface-version 'v1)
  (define (gen-dispatcher lookup-dispatcher)
    (lambda (conn req)
      (let* ([host (get-host (request-uri req) (request-headers req))])
        ((lookup-dispatcher host) conn req)))))