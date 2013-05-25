#;
(exn-pred exn:fail:contract?)
#lang racket

(module typed typed/racket
  (: d Any)
  (define d (delay (lambda: ([x : Integer]) (+ x 1))))
  (provide d))

(require 'typed)

;; this line should raise a ctc error
((force d) 6)

