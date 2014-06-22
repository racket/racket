#;
(exn-pred (regexp-quote "in: from"))
#lang typed/racket/base

;; Test for PR 14389. Make sure that the reported source location
;; points to the expression, not the whole submodule contents.

(require racket/flonum)
(module+ test
  (require typed/rackunit)
  (define (from upto)
    (for/flvector ([i (in-range from upto)])
      0.0)))
