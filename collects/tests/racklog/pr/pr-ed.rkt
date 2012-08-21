#lang racket
(require racklog)

(define %a
  (%rel (x)
        ((x) (%b x))
        ((x) (%c x))
        ))

(define %b
  (%rel ()
        ((1) !)
        ((2))))

(define %c
  (%rel ()
        ((2))))

(%find-all (x) (%a x))
