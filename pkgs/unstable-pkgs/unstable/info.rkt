#lang info

(define collection 'multi)

(define deps '("unstable-lib"
               "unstable-doc"
               "unstable-test"))
(define implies '("unstable-lib"
                  "unstable-doc"
                  "unstable-test"))

(define pkg-desc "Experimental libraries")

(define pkg-authors '(jay samth cce ryanc))
