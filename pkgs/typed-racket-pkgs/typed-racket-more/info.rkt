#lang info

(define collection 'multi)

(define deps '("base"
	       "net-lib"
               "draw-lib"
               "rackunit-lib"
               "typed-racket-lib"
               "gui-lib"))
(define build-deps '("scribble-lib"))

(define pkg-desc "Types for various libraries")

(define pkg-authors '(samth stamourv))
