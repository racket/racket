#lang info

(define collection 'multi)

(define deps '("base" 
               "scribble-lib"
               "string-constants-lib"
               "scribble-lib"
               "racket-index"
               "gui-lib"))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))

(define pkg-desc "Code implementing programmatic interfaces to some IDE tools that DrRacket supports")

(define pkg-authors '(robby))

(define version "1.0")
