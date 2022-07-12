#lang info

(define deps '("base"
               "scribble-lib"
               "at-exp-lib"
               "racket-doc"))

(define scribblings '(("zuo.scrbl" (multi-page) (language))))

(define pkg-desc "Documentation for the Zuo build language")

(define pkg-authors '(mflatt))

(define license '(Apache-2.0 OR MIT))
