#;#;
#<<END
TR opt: vector-set-quote.rkt 2:0 (vector-set! (ann (vector (quote (1 2))) (Vector Any)) 0 (quote (+ 1.0 2.0))) -- vector
END
""
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(vector-set! (ann (vector '(1 2)) (Vector Any))
             0
             '(+ 1.0 2.0)) ; we should not optimize under quote
