#lang info

(define collection 'multi)

(define deps '("base"
               "rackunit-lib"
               "at-exp-lib"
               "compatibility-lib"
               "drracket"
               "gui-lib"
               "pict-lib"
               "redex-lib"
               "scribble-lib"
	       "redex-examples"))
(define build-deps '())

(define pkg-desc "tests for \"redex\"")

(define pkg-authors '(robby bfetscher))
