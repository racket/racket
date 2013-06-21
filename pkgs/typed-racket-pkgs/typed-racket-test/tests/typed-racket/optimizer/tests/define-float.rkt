#;#;
#<<END
TR opt: define-float.rkt 10:10 (+ 1.0 2.0) -- binary float
END
""

#lang typed/scheme
#:optimize

(define x (+ 1.0 2.0))
