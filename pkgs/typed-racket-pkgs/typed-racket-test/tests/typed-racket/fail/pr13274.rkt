#;
(exn-pred exn:fail:contract?)
#lang racket/base

(module f-t typed/racket
  (: flomap-transform (case-> (Integer Integer -> Integer)
                              (Integer Integer Integer Integer Integer Integer -> Integer)))
  (define flomap-transform
    (case-lambda
      [(fm t)                             fm]
      [(fm t x-start y-start x-end y-end) fm]))
  (provide flomap-transform))

(require 'f-t)

(flomap-transform 2 3 4) ; only accepts 2 or 6 arguments
