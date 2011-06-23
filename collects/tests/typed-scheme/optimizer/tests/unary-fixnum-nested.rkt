#;
(
TR opt: unary-fixnum-nested.rkt 11:0 (bitwise-not (bitwise-not (length (quote (1 2 3))))) -- unary fixnum
TR opt: unary-fixnum-nested.rkt 11:13 (bitwise-not (length (quote (1 2 3)))) -- unary fixnum
3
)

#lang typed/scheme
#:optimize

(bitwise-not (bitwise-not (length '(1 2 3))))
