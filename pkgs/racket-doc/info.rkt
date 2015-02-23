#lang info

(define collection 'multi)

(define deps '("scheme-lib"
               "base"
               "net-lib"
               "sandbox-lib"
               "scribble-lib"
               "racket-index"))
(define build-deps '("rackunit-doc"
                     "compatibility"
                     "errortrace-doc"
                     "typed-racket-doc"
                     "at-exp-lib"
                     "rackunit-lib"
                     "web-server-doc"
                     "gui"
                     "draw"
                     "pict"
                     "parser-tools-doc"
                     "slideshow-doc"
                     "r5rs-doc"
                     "r6rs-doc"
                     "xrepl"
                     "readline"
                     "syntax-color"
                     "scribble-doc"
                     "future-visualizer"
                     "distributed-places"
                     "serialize-cstruct-lib"
                     "cext-lib"
                     "data-doc"
                     "net-doc"
                     "planet-doc"
                     "mzscheme-doc"
                     "compiler-lib"))

(define pkg-desc "Base Racket documentation")

(define pkg-authors '(eli jay matthias mflatt robby ryanc samth))
