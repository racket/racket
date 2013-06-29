#lang setup/infotab

(define collection "math")

(define scribblings '(["scribblings/math.scrbl" (multi-page)]))

(define compile-omit-paths '("tests"))
(define deps '("base"
               "typed-racket-lib"
               "typed-racket-more"))
(define build-deps '("at-exp-lib"
                     "gui-lib"
                     "plot"
                     "sandbox-lib"
                     "scribble-lib"))
