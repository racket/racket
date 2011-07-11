#;
(
TR opt: unary-fixnum-nested.rkt 12:0 (bitwise-not (bitwise-not (length (quote (1 2 3))))) -- unary fixnum
TR opt: unary-fixnum-nested.rkt 12:13 (bitwise-not (length (quote (1 2 3)))) -- unary fixnum
TR opt: unary-fixnum-nested.rkt 12:26 (length (quote (1 2 3))) -- known-length list length
3
)

#lang typed/scheme
#:optimize

(bitwise-not (bitwise-not (length '(1 2 3))))
