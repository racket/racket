#lang racket/load
(module p typed/racket
  (struct: (a) p ((v : a)))
  (provide p p-v))

(module p2 typed/racket
  (struct: (a) p ((v : a)))
  (provide p))


(module m typed/racket
  (require (only-in 'p p-v) 'p2)
  (p-v (p 0)))

(require 'm)
