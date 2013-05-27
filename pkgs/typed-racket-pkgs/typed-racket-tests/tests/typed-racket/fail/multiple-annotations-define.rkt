#;
(exn-pred 1 "Body had type:.*Variable had type:.*")
#lang typed/racket


(: a Integer)
(: b String)
(define-values (a b c) (values 1 2 3))
