#;
(
TR opt: invalid-exact-inexact.rkt 9:0 (exact->inexact 1.0) -- float to float
1.0
)

#lang typed/scheme
#:optimize
(exact->inexact 1.0) ; not an integer, can't optimize
