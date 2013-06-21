#;
#<<END
TR opt: n-ary-float.rkt 11:0 (+ 1.0 2.0 3.0) -- binary float
6.0

END

#lang typed/scheme
#:optimize

(+ 1.0 2.0 3.0)
