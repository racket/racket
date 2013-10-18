#lang info

(define collection 'multi)

(define deps '("base"
               "rackunit-lib"
               "at-exp-lib"
               "compatibility-lib"
               "drracket"
               "gui-lib"
               "pict-lib"
               "redex-gui-lib"
               "scribble-lib"
               "redex-examples"
               "data-lib"))
(define build-deps '("scheme-lib"))

(define pkg-desc "tests for \"redex\"")

(define pkg-authors '(robby bfetscher))
