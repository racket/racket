#;#;
#<<END
TR opt: double-float.rkt 13:0 (+ 2.0 2.0 2.0) -- binary float
END
#<<END
6.0

END

#lang typed/scheme
#:optimize

(+ 2.0 2.0 2.0)
