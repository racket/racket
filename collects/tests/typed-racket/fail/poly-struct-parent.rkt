#lang typed/racket

(define-struct: (A) Box ([value : A]) #:transparent)
(define-struct: (A) (Child-Box Box) () #:transparent)


(ann (Box-value (Child-Box 'sym)) Nothing)

