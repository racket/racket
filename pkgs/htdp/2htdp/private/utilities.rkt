#lang racket/base

(require racket/contract)

(provide/contract
 ;; like the unix debugging facility
 [tee (-> symbol? any/c any)]
 )


(define (tee tag x)
  (printf "~a ~s\n" tag x)
  x)
