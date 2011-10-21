#lang racket/load

(module m typed/racket
  (provide f)
  (define: f : Any '(a (2 3))))

(module n racket
  (require 'm)
  (list? (second f)))

(require 'n)
