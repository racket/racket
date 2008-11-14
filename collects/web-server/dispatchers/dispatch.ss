#lang scheme/base
(require scheme/contract)
(require "../private/connection-manager.ss"
         web-server/http)

(define dispatcher/c
  (connection? request? . -> . void))
(define dispatcher-interface-version/c
  (symbols 'v1))
(define-struct exn:dispatcher ())
(define (next-dispatcher) (raise (make-exn:dispatcher)))

(provide/contract
 [dispatcher/c contract?]
 [dispatcher-interface-version/c contract?]
 [next-dispatcher (-> void)]
 [struct exn:dispatcher ()])
