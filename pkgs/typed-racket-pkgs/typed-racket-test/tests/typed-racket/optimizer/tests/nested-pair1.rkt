#;#;
#<<END
TR opt: nested-pair1.rkt 2:0 (car (cdr (quote (1 2)))) -- pair
TR opt: nested-pair1.rkt 2:5 (cdr (quote (1 2))) -- pair
END
#<<END
2

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(car (cdr '(1 2)))
