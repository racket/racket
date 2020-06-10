#lang info

(define collection 'multi)

(define deps '("scheme-lib"
               ["base" #:version "6.5.0.2"]
               "net-lib"
               "sandbox-lib"
               ["scribble-lib" #:version "1.34"]
               "racket-index"))
(define build-deps '("rackunit-doc"
                     "compatibility-lib"
                     "errortrace-doc"
                     "at-exp-lib"
                     "rackunit-lib"
                     "gui-doc"
                     "gui-lib"
                     "draw-doc"
                     "draw-lib"
                     "pict-lib"
                     "parser-tools-doc"
                     "xrepl"
                     "readline"
                     ;"syntax-color-doc"
                     "syntax-color-lib"
                     ;"scribble-doc"
                     "future-visualizer-pict"
                     "distributed-places"
                     "serialize-cstruct-lib"
                     "cext-lib"
                     "net-doc"
                     "planet-doc"
                     "mzscheme-doc"
                     "compiler-lib"))

(define pkg-desc "Base Racket documentation")

(define pkg-authors '(eli jay matthias mflatt robby ryanc samth))

(define version "1.2")
