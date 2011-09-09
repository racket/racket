#;
(
TR opt: exact-inexact.rkt 10:0 (exact->inexact (expt 10 100)) -- int to float
1e+100
)

#lang typed/scheme
#:optimize

(exact->inexact (expt 10 100)) ; must not be a fixnum
