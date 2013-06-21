#;
#<<END
TR opt: let-rhs.rkt 13:9 (+ 1.0 2.0) -- binary float
3.0

END

#lang typed/scheme
#:optimize



(let ((x (+ 1.0 2.0)))
  x)
