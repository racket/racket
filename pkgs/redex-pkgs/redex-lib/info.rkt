#lang info

(define collection 'multi)

(define deps '("base"
               "draw-lib"
               "gui-lib"
               "data-lib"
               "profile-lib"
               "pict-lib"))
(define build-deps '("rackunit-lib"))

(define pkg-desc "implementation (no documentation) part of \"redex\"")

(define pkg-authors '(robby bfetscher))
