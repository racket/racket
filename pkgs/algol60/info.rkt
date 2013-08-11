#lang info

(define collection "algol60")
(define build-deps '("at-exp-lib"
                     "rackunit-lib"
                     "racket-doc"
                     "scribble-doc"
                     "scribble-lib"
                     "drracket-plugin-lib"))

(define tools '(("tool.rkt")))
(define tool-names '("Algol 60"))
(define scribblings '(("algol60.scrbl" () (experimental 40))))
(define deps '("base"
               "compatibility-lib"
               "drracket-plugin-lib"
               "errortrace-lib"
               "gui-lib"
               "parser-tools-lib"
               "string-constants-lib"))

(define pkg-desc "An implementation of the Algol60 language")

(define pkg-authors '(mflatt robby))
