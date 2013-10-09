#lang info

(define collection 'multi)

(define deps '("base"
               "plot-lib"
               "gui-lib"
               "snip-lib"
               "typed-racket-lib"
               "typed-racket-more"
               "unstable-lib"
               "unstable-contract-lib"
               "unstable-latent-contract-lib"
               "unstable-parameter-group-lib"))

(define build-deps '())

(define pkg-desc "Plot GUI interface")

(define pkg-authors '(ntoronto))
