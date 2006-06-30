(module dispatch-host mzscheme
  (require (lib "contract.ss"))
  (require "dispatch.ss"
           "../servlet-helpers.ss")
  (provide/contract
   [interface-version dispatcher-interface-version?]
   [make ((symbol? . -> . dispatcher?) . -> . dispatcher?)])
  
  (define interface-version 'v1)
  (define ((make lookup-dispatcher) conn req)
    (define host (get-host (request-uri req) (request-headers/raw req)))
    ((lookup-dispatcher host) conn req)))