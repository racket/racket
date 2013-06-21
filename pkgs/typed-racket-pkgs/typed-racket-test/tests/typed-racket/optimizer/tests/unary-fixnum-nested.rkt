#;#;
#<<END
TR opt: unary-fixnum-nested.rkt 15:0 (bitwise-not (bitwise-not (length (quote (1 2 3))))) -- unary fixnum
TR opt: unary-fixnum-nested.rkt 15:13 (bitwise-not (length (quote (1 2 3)))) -- unary fixnum
TR opt: unary-fixnum-nested.rkt 15:26 (length (quote (1 2 3))) -- known-length list length
END
#<<END
3

END

#lang typed/scheme
#:optimize

(bitwise-not (bitwise-not (length '(1 2 3))))
