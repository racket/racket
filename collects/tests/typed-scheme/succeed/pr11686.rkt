#lang racket/load

(module T typed/racket

  (struct: [X] doll ([contents : X]))

  (define-type RussianDoll
    (Rec RD (U 'center (doll RD))))

  (: f (RussianDoll -> RussianDoll))
  (define (f rd) rd)

  (provide (all-defined-out)))

(require 'T)
