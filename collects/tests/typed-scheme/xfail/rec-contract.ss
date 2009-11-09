#lang scheme/load

(module m typed-scheme
  (: f (Rec X (Number -> X)))
  (define (f n) f )
  (provide f)
  )

(module mm scheme
  (require 'm)
  (f 1))

(require 'mm)
