(module dispatch mzscheme
  (require "../connection-structs.ss"
           "../request-structs.ss"
           "../response-structs.ss")
  (require (lib "contract.ss")
           (lib "list.ss"))
  
  (provide dispatcher?)
  (define dispatcher? (connection? request? . -> . response?))
  
  (provide next-dispatcher
           [struct exn:dispatcher ()])
  (define-struct exn:dispatcher ())
  (define (next-dispatcher) (raise (make-exn:dispatcher))))