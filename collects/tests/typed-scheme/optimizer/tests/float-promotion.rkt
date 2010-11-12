#;
(
float-promotion.rkt line 13 col 4 - modulo - binary nonzero fixnum
float-promotion.rkt line 13 col 1 - + - binary float
float-promotion.rkt line 14 col 1 - + - binary float
2.0
1e+200
)

#lang typed/scheme
#:optimize

(+ (modulo 1 1) 2.0)
(+ (expt 100 100) 2.0)
