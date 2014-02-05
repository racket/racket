#;#;
#<<END
TR opt: literal-int.rkt 4:0 (+ 1 2.0) -- binary float
END
#<<END
3.0
1

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port



(+ 1 2.0)
1
