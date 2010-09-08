#;
(
0.5
)

#lang typed/scheme
#:optimize
(/ 1 2.0) ; result is not a float, can't optimize
