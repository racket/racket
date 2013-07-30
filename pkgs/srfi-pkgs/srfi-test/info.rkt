#lang info

(define collection 'multi)
(define deps '())
(define build-deps '("base"
                     "compatibility-lib"
                     "rackunit-lib"
                     "srfi-lib"))

(define pkg-desc "tests for \"srfi\"")

(define pkg-authors '(mflatt noel chongkai jay))
