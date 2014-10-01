#lang info

(define collection 'multi)

(define deps '("base" 
               "scribble-lib"
               "string-constants-lib"
               "scribble-lib"
               "racket-index"))
(define build-deps '("at-exp-lib"))

(define pkg-desc "Code implementing programmatic interfaces to some IDE tools that DrRacket supports")

(define pkg-authors '(robby))

(define version "1.0")
