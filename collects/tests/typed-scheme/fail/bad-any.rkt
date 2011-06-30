#;
(exn-pred exn:fail:contract?)
#lang scheme/load

(module m typed-scheme
  (: f Any)
  (define f (lambda: ([x : Number]) (add1 x)))
  (provide f))

(module n scheme
  (require 'm)
  (f "foo"))

(require 'n)
