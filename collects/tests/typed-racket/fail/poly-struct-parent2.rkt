#;
("Could not instantiate parent struct type")
#lang typed/racket

(define-struct: (A) Box ([value : A]) #:transparent)
(define-struct: (Child-Box Box) () #:transparent)

