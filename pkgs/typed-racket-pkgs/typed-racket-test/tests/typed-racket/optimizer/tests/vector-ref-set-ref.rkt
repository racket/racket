#;
#<<END
TR opt: vector-ref-set-ref.rkt 16:0 (vector-ref x 0) -- vector
TR opt: vector-ref-set-ref.rkt 17:0 (vector-set! x 1 "2") -- vector
TR opt: vector-ref-set-ref.rkt 18:0 (vector-ref x 1) -- vector
1
"2"

END

#lang typed/scheme
#:optimize

(: x (Vector Integer String))
(define x (vector 1 "1"))
(vector-ref x 0)
(vector-set! x 1 "2")
(vector-ref x 1)
