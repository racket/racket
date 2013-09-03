#;
(exn-pred "identifier bound to a structure type")
#lang racket

(module a racket
  (provide struct:posn make-posn posn? posn-x posn-y
           ;; this confuses require/typed
           [rename-out (posn-thing posn)])

  (struct posn (x y) #:transparent)
  (define (make-posn x y) (posn x y))
  (define posn-thing 0))

(module b typed/racket
  (require/typed (submod ".." a)
                 [#:struct posn ([x : Real] [y : Real])]))
