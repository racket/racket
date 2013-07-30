#lang info

(define collection 'multi)
(define deps '())
(define build-deps '("base"
                     "htdp-lib"
                     "compatibility-lib"
                     "gui-lib"
                     "racket-test"
                     "rackunit-lib"
                     "wxme-lib"))

(define pkg-desc "tests for \"htdp\"")

(define pkg-authors '(matthias mflatt robby))
