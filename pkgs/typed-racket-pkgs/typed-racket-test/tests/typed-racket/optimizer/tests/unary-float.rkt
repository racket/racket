#;
#<<END
TR opt: unary-float.rkt 11:0 (sin 2.0) -- unary float
0.9092974268256817

END

#lang typed/scheme
#:optimize

(sin 2.0)
