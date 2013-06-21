#;#;
#<<END
TR info: define-begin-float.rkt 12:18 display -- hidden parameter
TR opt: define-begin-float.rkt 12:26 (- 2.0 3.0) -- binary float
TR opt: define-begin-float.rkt 13:17 (* 2.0 3.0) -- binary float

END
"-1.0"

#lang typed/scheme
#:optimize
(define a (begin (display (- 2.0 3.0))
                 (* 2.0 3.0)))
