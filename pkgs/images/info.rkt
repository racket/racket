#lang setup/infotab

(define single-collection "images")

(define deps '("typed-racket-lib"))
(define build-deps '("racket-doc"
                     "unstable-latent-contract-lib"
                     "unstable-parameter-group-lib"))

(define scribblings '(["scribblings/images.scrbl" (multi-page) (gui-library)]))

(define compile-omit-paths '("tests"))
