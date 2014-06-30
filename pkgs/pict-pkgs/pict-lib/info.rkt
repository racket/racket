#lang info

(define collection 'multi)

(define deps '("scheme-lib"
               "base"
               "compatibility-lib"
               "draw-lib" "snip-lib"))
(define build-deps '("rackunit-lib"))

(define pkg-desc "implementation (no documentation) part of \"pict\"")

(define pkg-authors '(mflatt robby))

(define version "1.2")
