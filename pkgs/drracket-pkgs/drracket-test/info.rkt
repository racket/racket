#lang info

(define collection 'multi)

(define deps '("base"
               "htdp"
               "drracket"))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"
                     "compatibility-lib"
                     "gui-lib"
                     "htdp"
                     "scribble-lib"
                     "string-constants-lib"))

(define pkg-desc "tests for \"drracket\"")

(define pkg-authors '(robby))
