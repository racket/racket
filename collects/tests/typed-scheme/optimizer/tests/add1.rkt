#;
(
add1.rkt line 14 col 6 - add1 - fixnum add1
add1.rkt line 15 col 6 - sub1 - fixnum sub1
add1.rkt line 16 col 6 - add1 - float add1
add1.rkt line 17 col 6 - sub1 - float sub1
6
2
3.3
1.25
)

#lang typed/racket #:optimize
(ann (add1 5) Fixnum)
(ann (sub1 3) Fixnum)
(ann (add1 2.3) Float)
(ann (sub1 2.25) Float)
