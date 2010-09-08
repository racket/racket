#;
(
fx-fl.rkt line 10 col 1 - exact->inexact - fixnum to float
1.0
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(exact->inexact 1)
