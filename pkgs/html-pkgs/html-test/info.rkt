#lang info

(define collection 'multi)

(define deps '())

(define pkg-desc "tests for \"html\"")

(define pkg-authors '(jay mflatt))

(define build-deps 
  '("base" "html-lib" "rackunit-lib" "scribble-lib"))
