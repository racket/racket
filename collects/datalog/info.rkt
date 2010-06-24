#lang setup/infotab
(define name "Datalog")
(define blurb
  (list "An implementation of Datalog as a Racket language."))
(define scribblings '(["scribblings/datalog.scrbl" (multi-page)]))
(define categories '(devtools))
(define primary-file "main.rkt")
(define compile-omit-paths '("tests"))
(define release-notes (list))
(define repositories '("4.x"))
