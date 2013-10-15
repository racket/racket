#lang info

(define collection 'multi)

(define deps '("base"
               "draw-lib"
               "gui-lib"
               "snip-lib"
               "htdp-lib"))
(define build-deps '("racket-doc"
                     "htdp-doc"
                     "scribble-lib"))

(define pkg-desc "Teaching libraries for _Picturing Programs_")

(define pkg-authors '(sbloch))
