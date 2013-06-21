#;#;
#<<END
TR opt: rational-literal.rkt 11:0 (+ 3/4 1.2) -- binary float
END
#<<END
1.95

END
#lang typed/racket #:optimize
;; rational literals should be promoted to floats at compile time
(+ 3/4 1.2)
