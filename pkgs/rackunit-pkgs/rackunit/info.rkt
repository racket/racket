#lang info

(define collection 'multi)

(define deps '("rackunit-lib"
               "rackunit-doc"
               "rackunit-gui"
               "rackunit-plugin-lib"))
(define implies '("rackunit-lib"
                  "rackunit-doc"
                  "rackunit-gui"
                  "rackunit-plugin-lib"))

(define pkg-desc "RackUnit testing framework")

(define pkg-authors '(ryanc noel))
