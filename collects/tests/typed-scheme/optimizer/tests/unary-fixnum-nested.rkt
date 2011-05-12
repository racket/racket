#;
(
unary-fixnum-nested.rkt 11:14 bitwise-not -- unary fixnum
unary-fixnum-nested.rkt 11:1 bitwise-not -- unary fixnum
3
)

#lang typed/scheme
#:optimize

(bitwise-not (bitwise-not (length '(1 2 3))))
