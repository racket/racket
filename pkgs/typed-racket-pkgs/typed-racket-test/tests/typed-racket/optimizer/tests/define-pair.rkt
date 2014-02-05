#;#;
#<<END
TR opt: define-pair.rkt 2:10 (car (quote (1 3))) -- pair
END
""
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(define x (car '(1 3)))
