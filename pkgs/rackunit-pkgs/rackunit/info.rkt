#lang info

(define collection 'multi)

(define deps '("rackunit-lib"
               "rackunit-doc"
               "rackunit-gui"
               "rackunit-plugin-lib"
               "rackunit-test"))
(define implies '("rackunit-lib"
                  "rackunit-doc"
                  "rackunit-gui"
                  "rackunit-plugin-lib"
                  "rackunit-test"))

(define pkg-desc "RackUnit testing framework")

(define pkg-authors '(ryanc noel))
