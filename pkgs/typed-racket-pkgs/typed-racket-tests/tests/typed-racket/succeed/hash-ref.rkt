#lang scheme/load

(module m typed-scheme
  (define x ({inst make-hash Symbol Number}))
  (hash-ref! x 'key (lambda () 1))
  (hash-ref x 'key (lambda () 7))
  (provide x))

(module n scheme
  (require 'm)
  (hash-ref x 'key))

(require 'n)
