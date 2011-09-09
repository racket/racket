#;
(
TR opt: fx-fl.rkt 10:0 (exact->inexact 1) -- fixnum to float
1.0
)

#lang typed/scheme
#:optimize

(exact->inexact 1)
