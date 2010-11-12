#;
(
exact-inexact.rkt line 10 col 1 - exact->inexact - int to float
1e+100
)

#lang typed/scheme
#:optimize

(exact->inexact (expt 10 100)) ; must not be a fixnum
