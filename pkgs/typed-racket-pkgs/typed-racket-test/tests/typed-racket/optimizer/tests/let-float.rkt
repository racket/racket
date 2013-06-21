#;#;
#<<END
TR opt: let-float.rkt 14:9 (+ 3.0 2.0) -- binary float
TR opt: let-float.rkt 15:2 (* 9.0 x) -- binary float
END
#<<END
45.0

END

#lang typed/scheme
#:optimize

(let ((x (+ 3.0 2.0)))
  (* 9.0 x))
