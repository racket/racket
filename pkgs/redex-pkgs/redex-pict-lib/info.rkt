#lang info

(define collection 'multi)

(define deps '("scheme-lib"
               "base"
               "draw-lib"
               "data-lib"
               "profile-lib"
               "unstable-2d"
               "redex-lib"
               "pict-lib"))

(define build-deps '("rackunit-lib"))

(define implies '("redex-lib"))

(define pkg-desc "implementation (no documentation) part of \"redex\" using picts")

(define pkg-authors '(robby bfetscher))
