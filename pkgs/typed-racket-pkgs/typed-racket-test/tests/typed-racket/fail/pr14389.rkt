#;
(exn-pred (regexp-quote "in: (for/flvector"))
#lang typed/racket/base

(require racket/flonum)
(module+ test
  (require typed/rackunit)
  (define (from upto)
    (for/flvector ([i (in-range from upto)])
      0.0)))
