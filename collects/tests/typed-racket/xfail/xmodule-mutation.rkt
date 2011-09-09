#lang racket/load

(module m typed/racket
  (: x Any)
  (define x "foo")
  (: f (-> Void))
  (define (f) (set! x 1))
  (provide f x))

(module n typed/racket
  (require 'm)
  (if (string? x)
      (begin
        (f)
        ;; this should be a type error!
        (string-append x "foo"))
      0))

(require 'n)
