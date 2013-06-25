#;
(exn-pred 1)

#lang typed/racket/base
(define-type Hole (Rec Hole (U Number Hole)))
(ann "aaa" Hole)
