#;
#<<END
TR opt: nested-float.rkt 12:7 (+ 3.0 4.0) -- binary float
TR opt: nested-float.rkt 12:0 (+ 2.0 (+ 3.0 4.0)) -- binary float
9.0

END

#lang typed/scheme
#:optimize

(+ 2.0 (+ 3.0 4.0))
