#;
("Could not instantiate parent struct type")
#lang typed/racket

(define-struct: (A) Box ([value : A]))
(define-struct: (Child-Box Box) ())

