#;
#<<END
TR opt: begin-float.rkt 13:7 (- 2.0 3.0) -- binary float
TR opt: begin-float.rkt 14:7 (* 2.0 3.0) -- binary float
-1.0
6.0

END

#lang typed/scheme
#:optimize

(begin (- 2.0 3.0)
       (* 2.0 3.0))
