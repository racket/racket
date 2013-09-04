#lang info

(define collection 'multi)

(define deps '("scheme-lib"
               "base"
               "draw-lib"
               "gui-lib"
               "data-lib"
               "profile-lib"
               "unstable-2d"
               "pict-lib"))
(define build-deps '("rackunit-lib"))

(define pkg-desc "implementation (no documentation) part of \"redex\"")

(define pkg-authors '(robby bfetscher))
