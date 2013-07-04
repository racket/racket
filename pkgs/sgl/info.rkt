#lang info

(define collection "sgl")

(define compile-omit-paths '("examples"))

(define scribblings '(("scribblings/sgl.scrbl" (multi-page) (gui-library))))
(define deps '("base"
               "compatibility-lib"
               "gui-lib"))
(define build-deps '("scribble-lib"))
