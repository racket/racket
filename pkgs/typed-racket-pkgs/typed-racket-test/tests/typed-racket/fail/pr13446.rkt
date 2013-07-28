#;
(exn-pred #rx"blaming: top-level")
#lang racket/load

(module a typed/racket
  (provide p)
  (: p (Parameterof String Index))
  (define p (make-parameter 0 string-length)))

(require 'a)
(p 0)

