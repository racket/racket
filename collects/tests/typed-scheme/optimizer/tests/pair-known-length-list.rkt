#;
(
pair-known-length-list.rkt line 27 col 1 - car - pair
pair-known-length-list.rkt line 28 col 1 - cdr - pair
pair-known-length-list.rkt line 29 col 6 - cdr - pair
pair-known-length-list.rkt line 29 col 1 - car - pair
pair-known-length-list.rkt line 30 col 6 - cdr - pair
pair-known-length-list.rkt line 30 col 1 - cdr - pair
pair-known-length-list.rkt line 31 col 11 - cdr - pair
pair-known-length-list.rkt line 31 col 6 - cdr - pair
pair-known-length-list.rkt line 31 col 1 - car - pair
pair-known-length-list.rkt line 32 col 11 - cdr - pair
pair-known-length-list.rkt line 32 col 6 - cdr - pair
pair-known-length-list.rkt line 32 col 1 - cdr - pair
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
