#;
(
TR opt: box.rkt 17:1 unbox -- box
TR opt: box.rkt 18:1 set-box! -- box
TR opt: box.rkt 19:1 unbox -- box
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
