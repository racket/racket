#;
(
TR opt: vector-ref-set-ref.rkt 15:1 vector-ref -- vector
TR opt: vector-ref-set-ref.rkt 16:1 vector-set! -- vector
TR opt: vector-ref-set-ref.rkt 17:1 vector-ref -- vector
1
"2"
)

#lang typed/scheme
#:optimize

(: x (Vector Integer String))
(define x (vector 1 "1"))
(vector-ref x 0)
(vector-set! x 1 "2")
(vector-ref x 1)
