#;
(
invalid-exact-inexact.rkt line 9 col 1 - exact->inexact - float to float
1.0
)

#lang typed/scheme
#:optimize
(exact->inexact 1.0) ; not an integer, can't optimize
