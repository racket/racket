#lang info

(define collection "racklog")

(define scribblings
  '(("racklog.scrbl" (multi-page) (tool))))
(define deps '("base"
               "datalog"))
(define build-deps '("eli-tester"
                     "rackunit-lib"
                     "racket-doc"
                     "scribble-lib"))
