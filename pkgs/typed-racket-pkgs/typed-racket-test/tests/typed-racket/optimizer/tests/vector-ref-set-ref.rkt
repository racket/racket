#;#;
#<<END
TR opt: vector-ref-set-ref.rkt 4:0 (vector-ref x 0) -- vector
TR opt: vector-ref-set-ref.rkt 5:0 (vector-set! x 1 "2") -- vector
TR opt: vector-ref-set-ref.rkt 6:0 (vector-ref x 1) -- vector
END
#<<END
1
"2"

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(: x (Vector Integer String))
(define x (vector 1 "1"))
(vector-ref x 0)
(vector-set! x 1 "2")
(vector-ref x 1)
