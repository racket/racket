#lang eopl

(require "data-structures.rkt")
(provide init-nameless-env empty-nameless-env extend-nameless-env
         apply-nameless-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> Nameless-env

;; (init-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10.  

(define init-nameless-env
  (lambda ()
    (extend-nameless-env 
     (num-val 1)                        ; was i
     (extend-nameless-env
      (num-val 5)                       ; was v
      (extend-nameless-env
       (num-val 10)                     ; was x
       (empty-nameless-env))))))
