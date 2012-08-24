#;
("Could not instantiate parent struct type")
#lang typed/racket

(define-struct: (A B) Box ([value : A]
                           [other : B]))
(define-struct: (C) (Child-Box Box) ())


