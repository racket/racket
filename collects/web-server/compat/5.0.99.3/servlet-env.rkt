#lang racket/base
(require (prefix-in new: web-server/servlet-env)
         "../http/response-structs.rkt")

(define serve/servlet
  (make-keyword-procedure
   (lambda (kws kw-args gen)
     (keyword-apply new:serve/servlet
                    kws
                    kw-args
                    (λ (req) (normalize-response (gen req)))))))

(provide (all-defined-out))