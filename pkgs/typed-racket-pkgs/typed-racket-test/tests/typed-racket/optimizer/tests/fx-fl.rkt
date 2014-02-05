#;#;
#<<END
TR opt: fx-fl.rkt 2:0 (exact->inexact 1) -- fixnum to float
END
#<<END
1.0

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(exact->inexact 1)
