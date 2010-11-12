#;
(
unary-fixnum-nested.rkt line 11 col 6 - bitwise-not - unary fixnum
unary-fixnum-nested.rkt line 11 col 1 - abs - unary fixnum
4
)

#lang typed/scheme
#:optimize

(abs (bitwise-not (length '(1 2 3))))
