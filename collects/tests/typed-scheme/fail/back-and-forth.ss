#;
(exn-pred exn:fail:contract? #rx".*violator.*contract.*\\(-> Number Number\\).*")

#lang scheme/load

(module m typed/scheme
  (: f (Number -> Number))
  (define (f x) (add1 x))
  (provide f))

(module violator scheme
  (require 'm)
  (f 'foo))

(module o typed-scheme
  (require 'violator))

(require 'o)
