#lang typed/racket/base

(require "bigfloat-struct.rkt")

(provide bfbeta bflog-beta)

(: bflog-beta (Bigfloat Bigfloat -> Bigfloat))
(define (bflog-beta a b)
  (bf+ (bflog-gamma a)
       (bflog-gamma b)
       (bf- (bflog-gamma (bf+ a b)))))

(: bfbeta (Bigfloat Bigfloat -> Bigfloat))
(define (bfbeta a b)
  (bfexp (bflog-beta a b)))
