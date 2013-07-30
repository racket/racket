#lang info

(define collection 'multi)

(define deps '("base"
               "planet-lib"
               "compatibility-lib"
	       "net-lib"
               "distributed-places-lib"
               "sandbox-lib"
               "compiler-lib"
               "scribble-lib"
               "racket-index"))
(define build-deps '("compatibility-doc"
                     "errortrace-doc"
                     "typed-racket-doc"
                     "unstable"
                     "at-exp-lib"
                     "data-lib"
                     "pconvert-lib"
                     "rackunit-lib"
                     "unstable-contract-lib"
                     "web-server"
                     "gui"
                     "draw"
                     "sandbox-lib"
                     "pict"
                     "parser-tools"
                     "slideshow"
                     "r5rs"
                     "r6rs"
                     "xrepl"
                     "readline"
                     "profile"
                     "syntax-color"
                     "scribble"
                     "compatibility-lib"
                     "future-visualizer"
                     "distributed-places-doc"))

(define pkg-desc "Base Racket documentation")

(define pkg-authors '(eli jay matthias mflatt robby ryanc samth))
