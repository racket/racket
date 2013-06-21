#;
#<<END
TR opt: double-float.rkt 11:0 (+ 2.0 2.0 2.0) -- binary float
6.0

END

#lang typed/scheme
#:optimize

(+ 2.0 2.0 2.0)
