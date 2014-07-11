#lang racket/base
(require racket/contract
         unstable/contract)

(provide if-first-order-passes/c)

(define (if-first-order-passes/c ctc)
  (if/c (contract-first-order ctc)
        ctc
        any/c))
