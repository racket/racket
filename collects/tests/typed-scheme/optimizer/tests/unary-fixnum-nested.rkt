#;
(
TR opt: unary-fixnum-nested.rkt 12:1 bitwise-not -- unary fixnum
TR opt: unary-fixnum-nested.rkt 12:14 bitwise-not -- unary fixnum
TR opt: unary-fixnum-nested.rkt 12:27 length -- known-length list length
3
)

#lang typed/scheme
#:optimize

(bitwise-not (bitwise-not (length '(1 2 3))))
