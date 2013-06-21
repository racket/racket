#;
#<<END
TR opt: unary-fixnum-nested.rkt 13:26 (length (quote (1 2 3))) -- known-length list length
TR opt: unary-fixnum-nested.rkt 13:13 (bitwise-not (length (quote (1 2 3)))) -- unary fixnum
TR opt: unary-fixnum-nested.rkt 13:0 (bitwise-not (bitwise-not (length (quote (1 2 3))))) -- unary fixnum
3

END

#lang typed/scheme
#:optimize

(bitwise-not (bitwise-not (length '(1 2 3))))
