#lang racket/base
(require "export.rkt"
         "gensym.rkt")

(provide id-to-variable)

;; adds to extra-variables as needed
(define (id-to-variable int-id exports extra-variables)
  (export-id
   (or (hash-ref exports int-id #f)
       (and extra-variables
            (or (hash-ref extra-variables int-id #f)
                (let ([ex (export (deterministic-gensym int-id) int-id)])
                  (hash-set! extra-variables int-id ex)
                  ex))))))
