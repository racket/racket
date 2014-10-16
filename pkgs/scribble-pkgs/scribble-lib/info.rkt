#lang info

(define collection 'multi)

(define deps '("scheme-lib"
               "base"
               "compatibility-lib"
               "scribble-text-lib"
               "scribble-html-lib"
               "planet-lib" ; used dynamically
	       "net-lib"
               "at-exp-lib"
               "draw-lib" 
               "syntax-color-lib"
               "sandbox-lib"))
(define build-deps '("rackunit-lib"))

(define implies '("scribble-html-lib"))

(define pkg-desc "implementation (no documentation) part of \"scribble\"")

(define pkg-authors '(mflatt eli))

(define version "1.6")
