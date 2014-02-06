#;#;
#<<END
TR opt: simple-pair.rkt 2:0 (car (cons 1 2)) -- pair
END
#<<END
1

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(car (cons 1 2))
