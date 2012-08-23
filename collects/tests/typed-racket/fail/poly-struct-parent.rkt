#lang typed/racket

(define-struct: (A) Box ([value : A]))
(define-struct: (A) (Child-Box Box) ())


(ann (Box-value (Child-Box 'sym)) Nothing)

