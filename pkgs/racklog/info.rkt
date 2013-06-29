#lang setup/infotab

(define collection "racklog")

(define scribblings
  '(("racklog.scrbl" (multi-page) (tool))))
(define deps '("base"
               "datalog"))
(define build-deps '("scribble-lib"))
