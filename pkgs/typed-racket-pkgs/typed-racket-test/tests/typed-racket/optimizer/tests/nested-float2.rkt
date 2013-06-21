#;
#<<END
TR opt: nested-float2.rkt 12:7 (* 3.0 4.0) -- binary float
TR opt: nested-float2.rkt 12:0 (+ 2.0 (* 3.0 4.0)) -- binary float
14.0

END

#lang typed/scheme
#:optimize

(+ 2.0 (* 3.0 4.0))
