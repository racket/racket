#lang info

(define collection 'multi)

(define deps '("base"
               "draw-lib"
               "gui-lib"
               "snip-lib"
               "htdp"))
(define build-deps '("racket-doc"
                     "scribble-lib"))

(define pkg-desc "Teaching libraries for _Picturing Programs_")

(define pkg-authors '(sbloch))
