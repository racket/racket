(module dispatch mzscheme
  (require (lib "contract.ss"))
  (require "../connection-structs.ss"
           "../request-structs.ss"
           "../response-structs.ss")
  
  (define dispatcher? 
    (connection? request? . -> . void))
  (define dispatcher-interface-version?
    symbol?)
  (define-struct exn:dispatcher ())
  (define (next-dispatcher) (raise (make-exn:dispatcher)))
  
  (provide/contract
   [dispatcher? contract?]
   [dispatcher-interface-version? (any/c . -> . boolean?)]
   [next-dispatcher (-> void)]
   [struct exn:dispatcher ()]))