#lang setup/infotab

(define collection "redex")

(define name "PLT Redex")
(define scribblings (list (list "redex.scrbl" (list 'multi-page) (list 'tool))))
(define release-notes (list (list "Redex" "HISTORY.txt")))

(define compile-omit-paths '("tests"))
(define deps '("profile-lib"
               "rackunit-lib"
               "slideshow-lib"
               "base"
               "compiler-lib"
               "compatibility-lib"
               "data-lib"
               "draw-lib"
               "gui-lib"
               "pict-lib"))
(define build-deps '("at-exp-lib"
                     "htdp"
                     "racket-doc"
                     "rackunit-lib"
                     "scribble-lib"))
