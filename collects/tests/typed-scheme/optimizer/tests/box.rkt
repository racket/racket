#;
(
TR opt: box.rkt 17:0 (unbox x) -- box
TR opt: box.rkt 18:0 (set-box! x 2) -- box
TR opt: box.rkt 19:0 (unbox x) -- box
1
2
)

#lang typed/scheme
#:optimize



(: x (Boxof Integer))
(define x (box 1))
(unbox x)
(set-box! x 2)
(unbox x)
