#;
("Could not instantiate parent struct type")
#lang typed/racket

(define-struct: (A B) Box ([value : A]
                           [other : B]) #:transparent)
(define-struct: (C) (Child-Box Box) () #:transparent)


