#;#;
#<<END
TR opt: let-float.rkt 2:9 (+ 3.0 2.0) -- binary float
TR opt: let-float.rkt 3:2 (* 9.0 x) -- binary float
END
#<<END
45.0

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(let ((x (+ 3.0 2.0)))
  (* 9.0 x))
