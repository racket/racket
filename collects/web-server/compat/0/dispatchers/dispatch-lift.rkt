#lang racket/base
(require (prefix-in new: web-server/dispatchers/dispatch-lift)
         "../http/response-structs.rkt")

(define (make d)
  (new:make (λ (req) (normalize-response (d req)))))

(provide 
 (rename-out
  [new:interface-version interface-version])
 (all-defined-out))