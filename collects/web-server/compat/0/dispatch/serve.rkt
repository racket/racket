#lang racket/base
(require (prefix-in new: web-server/dispatch/serve)
         "../http/response-structs.rkt")

(define (serve/dispatch d)
  (new:serve/dispatch (λ (req) (normalize-response (d req)))))

(provide (all-defined-out))