(module dispatch mzscheme
  (require (lib "contract.ss"))
  (require "../private/connection-manager.ss"
           "../private/request-structs.ss")
  
  (define dispatcher? 
    (connection? request? . -> . void))
  (define (dispatcher-interface-version? v)
    (and (symbol? v) (eq? v 'v1)))
  (define-struct exn:dispatcher ())
  (define (next-dispatcher) (raise (make-exn:dispatcher)))
  
  (provide/contract
   [dispatcher? contract?]
   [dispatcher-interface-version? (any/c . -> . boolean?)]
   [next-dispatcher (-> void)]
   [struct exn:dispatcher ()]))