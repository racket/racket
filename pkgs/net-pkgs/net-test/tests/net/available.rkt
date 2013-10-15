#lang racket/base
(require racket/tcp
         racket/list
         racket/match
         racket/port
         racket/contract)

(define (tcp-localhost-available?)
  (with-handlers
      ([exn? (λ (x) #f)])
    (define the-listener
      (tcp-listen 0 5 #t))
    (define-values (local-host port end-host end-port)
      (tcp-addresses the-listener #t))
    (thread
     (λ ()
       (tcp-accept the-listener)
       (tcp-close the-listener)))
    (tcp-connect "localhost" port)
    #t))

(provide
 (contract-out
  [tcp-localhost-available? (-> boolean?)]))

(module+ main
  (tcp-localhost-available?))

(module+ test (require (submod ".." main))) ; for raco test & drdr
