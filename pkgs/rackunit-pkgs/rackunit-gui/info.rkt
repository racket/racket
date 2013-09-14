#lang info

(define collection 'multi)

(define deps '("rackunit-lib"
               "data-lib"
               "gui-lib"
               "unstable-list-lib" ; for class-iop
               "base"))

(define pkg-desc "RackUnit test runner GUI")

(define pkg-authors '(ryanc))
