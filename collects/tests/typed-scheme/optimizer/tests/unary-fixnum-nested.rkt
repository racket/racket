#;
(
unary-fixnum-nested.rkt line 11 col 14 - bitwise-not - unary fixnum
unary-fixnum-nested.rkt line 11 col 1 - bitwise-not - unary fixnum
3
)

#lang typed/scheme
#:optimize

(bitwise-not (bitwise-not (length '(1 2 3))))
