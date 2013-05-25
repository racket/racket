#lang reader "dollar-racket.rkt"

(provide cost)

;; Cost of `n' $1 rackets with 7% sales
;; tax and shipping-and-handling fee `h':
(define (cost n h)
  $n*107/100+h$)
