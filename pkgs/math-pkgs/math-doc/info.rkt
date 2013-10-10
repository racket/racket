#lang info

(define collection 'multi)

(define deps '("base"))

(define build-deps '("at-exp-lib"
                     "math-lib"
                     "plot-doc"
                     "plot-gui-lib"
                     "racket-doc"
                     "sandbox-lib"
                     "scribble-lib"
                     "typed-racket-doc"
                     "typed-racket-lib"
                     "unstable-2d"))

(define pkg-desc "Math library documentation")

(define pkg-authors '(ntoronto))
