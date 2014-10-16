#lang info

(define collection 'multi)
(define deps '())
(define build-deps '("scheme-lib"
                     "base"
                     "compatibility-lib"
                     "rackunit-lib"
                     "srfi-lib"))
(define update-implies '("srfi-lib"))

(define pkg-desc "tests for \"srfi\"")

(define pkg-authors '(mflatt noel chongkai jay))
