#;
(exn-pred exn:fail:contract?)

#lang racket/load

(module T typed/racket

  (struct: [X] doll ([contents : X]))

  (define-type RussianDoll
    (Rec RD (U 'center (doll RD))))

  (: f (RussianDoll -> RussianDoll))
  (define (f rd) rd)

  (: md (All (x) (x -> (doll x))))
  (define md doll)

  (provide (all-defined-out)))

(module U racket
 (require 'T)
 (f (md 3)))

(require 'U)




