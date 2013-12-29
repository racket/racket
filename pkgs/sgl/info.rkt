#lang info

(define collection "sgl")

(define compile-omit-paths '("examples"))
(define test-omit-paths '("examples"))

(define scribblings '(("scribblings/sgl.scrbl" (multi-page) (gui-library))))
(define deps '("scheme-lib"
               "base"
               "compatibility-lib"
               "gui-lib"))
(define build-deps '("draw-doc"
                     "gui-doc"
                     "racket-doc"
                     "scribble-lib"))

(define pkg-desc "Legacy OpenGL library")

(define pkg-authors '(jay))
