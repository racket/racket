#lang info

(define collection 'multi)

(define deps
  '("base"
    "compatibility-lib"
    "draw-lib"
    "errortrace-lib"
    "gui-lib"
    "htdp-doc"
    "images"
    "pconvert-lib"
    "r5rs-lib"
    "sandbox-lib"
    "scribble-lib"
    "slideshow-lib"
    "snip-lib"    
    "string-constants-lib"
    "typed-racket-lib"
    "web-server-lib"
    "wxme-lib"
    "drracket"
    "deinprogramm"))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))
