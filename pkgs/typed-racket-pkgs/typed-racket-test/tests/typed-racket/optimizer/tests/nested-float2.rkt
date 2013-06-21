#;#;
#<<END
TR opt: nested-float2.rkt 14:0 (+ 2.0 (* 3.0 4.0)) -- binary float
TR opt: nested-float2.rkt 14:7 (* 3.0 4.0) -- binary float
END
#<<END
14.0

END

#lang typed/scheme
#:optimize

(+ 2.0 (* 3.0 4.0))
