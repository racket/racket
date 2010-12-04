#lang racket/base
(require (prefix-in new: web-server/dispatchers/dispatch-pathprocedure)
         "../http/response-structs.rkt")

(define (make p d)
  (new:make p (λ (req) (normalize-response (d req)))))

(provide 
 (rename-out
  [new:interface-version interface-version])
 (all-defined-out))