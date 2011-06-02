#;
(
invalid-sqrt.rkt 9:0 (#%app sqrt (quote -2.0)) -- unexpected complex value -- caused by: 9:1 sqrt
0+1.4142135623730951i
)

#lang typed/scheme
#:optimize
(sqrt -2.0) ; not a nonnegative flonum, can't optimize
