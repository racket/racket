#;#;
#<<END
TR opt: unary-fixnum.rkt 2:0 (bitwise-not 4) -- unary fixnum
TR opt: unary-fixnum.rkt 3:0 (- (ann 2 Nonnegative-Fixnum)) -- unary fixnum
END
#<<END
-5
-2
2

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(bitwise-not 4)
(- (ann 2 Nonnegative-Fixnum))
(- (ann -2 Negative-Fixnum)) ; can't optimize negatives
