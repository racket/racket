#;
(
pair-known-length-list.rkt 27:1 car -- pair
pair-known-length-list.rkt 28:1 cdr -- pair
pair-known-length-list.rkt 29:1 car -- pair
pair-known-length-list.rkt 29:6 cdr -- pair
pair-known-length-list.rkt 30:1 cdr -- pair
pair-known-length-list.rkt 30:6 cdr -- pair
pair-known-length-list.rkt 31:1 car -- pair
pair-known-length-list.rkt 31:6 cdr -- pair
pair-known-length-list.rkt 31:11 cdr -- pair
pair-known-length-list.rkt 32:1 cdr -- pair
pair-known-length-list.rkt 32:6 cdr -- pair
pair-known-length-list.rkt 32:11 cdr -- pair
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
