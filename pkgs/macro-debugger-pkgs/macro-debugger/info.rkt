#lang info

(define collection 'multi)

(define deps '("base"
               "compatibility-lib"
               "data-lib"
               "gui-lib"
               "images-lib"
               "images-gui-lib"
               "parser-tools-lib"
               "unstable-lib"
               "unstable-list-lib"
               "macro-debugger-text-lib"))
(define build-deps '("rackunit-lib"
                     "scribble-lib"
                     "racket-doc"
                     "unstable-doc"))

(define pkg-desc "The macro debugger tool")

(define pkg-authors '(ryanc))
