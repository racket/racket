#;
(
0.5
)

#lang typed/scheme
#:optimize
(/ (ann 1 Integer) 2.0) ; result is not a float, can't optimize
