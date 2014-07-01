#lang info

(define collection 'multi)

(define deps '("srfi-lite-lib"
               "base"
	       "net-lib"
               "draw-lib"
               "rackunit-lib"
               "rackunit-gui"
               "snip-lib"
               "typed-racket-lib"
               "gui-lib"
               "pict-lib"))

(define pkg-desc "Types for various libraries")

(define pkg-authors '(samth stamourv))

(define version "1.1")