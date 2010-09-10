#;
(
vector-ref-set-ref.rkt line 15 col 1 - vector-ref - vector
vector-ref-set-ref.rkt line 16 col 1 - vector-set! - vector
vector-ref-set-ref.rkt line 17 col 1 - vector-ref - vector
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
