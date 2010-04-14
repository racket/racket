#lang scheme/load

(module m typed-scheme
  (define-struct: (A) x ([v : A]))
  (provide make-x x-v))

(module n scheme
  (require 'm)
  (x-v (make-x 1)))

(require 'm)
(require 'n)
