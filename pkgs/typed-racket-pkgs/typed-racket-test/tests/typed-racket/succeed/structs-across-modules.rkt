#lang racket/load

(module t1 typed/racket
  (provide node)
  (struct: node ()))

(module t2 typed/racket
  (require 't1)
  (: v node)
  (define v (node)))
(module u racket
  (require 't1)
  (node))

(require 't2)
(require 'u)
