#lang scheme/load

(module m typed-scheme
  (: f (All (a) (a -> a)))
  (define (f x) x)
  (provide f))

(module n typed-scheme
  (require 'm))

(require typed-scheme)

(require 'n)

(current-namespace (module->namespace ''n))

(f 1)

