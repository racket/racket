#;
#<<END
TR opt: simple-float.rkt 11:0 (+ 2.0 3.0) -- binary float
5.0

END

#lang typed/scheme
#:optimize

(+ 2.0 3.0)
