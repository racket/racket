#lang racket/base
(require (prefix-in new: web-server/servlet-dispatch)
         "http/response-structs.rkt")

(define dispatch/servlet
  (make-keyword-procedure
   (lambda (kws kw-args gen)
     (keyword-apply new:dispatch/servlet
                    kws
                    kw-args
                    (λ (req) (normalize-response (gen req)))))))

(provide (rename-out [new:serve/launch/wait serve/launch/wait])
         (all-defined-out))