#lang info

(define collection "datalog")

(define scribblings '(["scribblings/datalog.scrbl" (multi-page) (language)]))

(define compile-omit-paths '("tests"))
(define deps '("base"
               "parser-tools-lib"))
(define build-deps '("scribble-lib"))
