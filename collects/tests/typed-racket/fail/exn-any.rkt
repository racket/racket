#;
(exn-pred "Any")
#lang racket/load

(module m typed/racket
  (struct: s ())
  
  (struct: s2 s ())
  (define:  v : Any (s2))
  (provide v))

(module n racket
  (require 'm)
  v)

(require 'n)

