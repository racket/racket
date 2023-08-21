#lang info

(define collection 'multi)

(define deps '("scheme-lib"
               ["base" #:version "6.5.0.2"]
               "net-lib"
               "sandbox-lib"
               ["scribble-lib" #:version "1.34"]
               "racket-index"))
(define build-deps '("rackunit-doc"
                     "errortrace-doc"
                     "at-exp-lib"
                     "rackunit-lib"
                     "gui-doc"
                     "gui-lib"
                     "draw-doc"
                     "draw-lib"
                     "pict-lib"
                     "readline-lib"
                     "readline-doc"
                     "syntax-color-doc"
                     "syntax-color-lib"
                     "scribble-doc"
                     ["future-visualizer" #:version "1.1"]
                     "distributed-places-doc"
                     "distributed-places-lib"
                     "serialize-cstruct-lib"
                     "cext-lib"
                     "net-doc"
                     "planet-doc"
                     "compiler-lib"
                     "compatibility-lib"
                     "xrepl-lib"
                     "xrepl-doc"))

(define pkg-desc "Base Racket documentation")

(define pkg-authors '(jay matthias mflatt robby ryanc samth))

(define version "1.3")

(define license
  '(Apache-2.0 OR MIT))
