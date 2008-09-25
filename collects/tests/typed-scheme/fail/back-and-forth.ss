#;
(exn-pred exn:fail:contract? #rx".*contract.*\\(-> number\\? number\\?\\).*")

#lang scheme/load

(module m typed-scheme
  (: f (Number -> Number))
  (define (f x) (add1 x))
  (provide f))

(module n scheme
  (require 'm)
  (f 'foo))

(module o typed-scheme
  (require 'n))

(require 'o)
