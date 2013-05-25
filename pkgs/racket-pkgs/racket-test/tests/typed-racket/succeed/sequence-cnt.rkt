#lang racket/load

(module m typed/racket
  (provide s)
  (: s (Sequenceof Integer))
  (define s (list 1 2 3)))

(module n racket
  (require 'm)
  (for ([i s]) (add1 i)))

(require 'n)
