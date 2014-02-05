#;#;
#<<END
END
#<<END
'(+ 1.0 2.0)

END

#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
'(+ 1.0 2.0)
