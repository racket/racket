#;
(
TR opt: pair-known-length-list.rkt 27:0 (car x) -- pair
TR opt: pair-known-length-list.rkt 28:0 (cdr x) -- pair
TR opt: pair-known-length-list.rkt 29:0 (car (cdr x)) -- pair
TR opt: pair-known-length-list.rkt 29:5 (cdr x) -- pair
TR opt: pair-known-length-list.rkt 30:0 (cdr (cdr x)) -- pair
TR opt: pair-known-length-list.rkt 30:5 (cdr x) -- pair
TR opt: pair-known-length-list.rkt 31:0 (car (cdr (cdr x))) -- pair
TR opt: pair-known-length-list.rkt 31:5 (cdr (cdr x)) -- pair
TR opt: pair-known-length-list.rkt 31:10 (cdr x) -- pair
TR opt: pair-known-length-list.rkt 32:0 (cdr (cdr (cdr x))) -- pair
TR opt: pair-known-length-list.rkt 32:5 (cdr (cdr x)) -- pair
TR opt: pair-known-length-list.rkt 32:10 (cdr x) -- pair
1
'(2 3)
2
'(3)
3
'()
)

#lang typed/racket #:optimize

(: x (List Integer Integer Integer))
(define x (list 1 2 3))
(car x)
(cdr x)
(car (cdr x))
(cdr (cdr x))
(car (cdr (cdr x)))
(cdr (cdr (cdr x)))
