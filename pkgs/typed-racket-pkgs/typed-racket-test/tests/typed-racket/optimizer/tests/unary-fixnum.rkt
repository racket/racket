#;
(
TR opt: unary-fixnum.rkt 13:0 (bitwise-not 4) -- unary fixnum
TR opt: unary-fixnum.rkt 14:0 (- (ann 2 Nonnegative-Fixnum)) -- unary fixnum
-5
-2
2
)

#lang typed/scheme
#:optimize

(bitwise-not 4)
(- (ann 2 Nonnegative-Fixnum))
(- (ann -2 Negative-Fixnum)) ; can't optimize negatives
