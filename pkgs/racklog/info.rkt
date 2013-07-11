#lang info

(define collection "racklog")

(define scribblings
  '(("racklog.scrbl" (multi-page) (tool))))
(define deps '("base"
               "datalog"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
