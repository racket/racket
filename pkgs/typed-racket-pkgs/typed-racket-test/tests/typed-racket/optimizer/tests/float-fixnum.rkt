#;#;
#<<END
TR opt: float-fixnum.rkt 15:0 (+ 1 2 3.0) -- binary float
TR opt: float-fixnum.rkt 15:0 (+ 1 2 3.0) -- fixnum bounded expr
TR opt: float-fixnum.rkt 16:0 (* 1 2 3.0) -- binary float
TR opt: float-fixnum.rkt 16:0 (* 1 2 3.0) -- fixnum bounded expr
END
#<<END
6.0
6.0

END
#lang typed/racket

(+ 1 2 3.0)
(* 1 2 3.0)
