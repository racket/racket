#;
#<<END
TR opt: unary-fixnum.rkt 14:0 (bitwise-not 4) -- unary fixnum
TR opt: unary-fixnum.rkt 15:0 (- (ann 2 Nonnegative-Fixnum)) -- unary fixnum
-5
-2
2

END

#lang typed/scheme
#:optimize

(bitwise-not 4)
(- (ann 2 Nonnegative-Fixnum))
(- (ann -2 Negative-Fixnum)) ; can't optimize negatives
