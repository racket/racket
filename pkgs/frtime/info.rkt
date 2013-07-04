#lang info

(define collection "frtime")

(define name "FrTime")

(define compile-omit-paths '("demos" "tests"))

(define scribblings '(("scribblings/frtime.scrbl" () (experimental 50))))
(define deps '("base"
               "compatibility-lib"
               "drracket"
               "gui-lib"
               "pict-lib"
               "string-constants-lib"))
(define build-deps '("scribble-lib"))
