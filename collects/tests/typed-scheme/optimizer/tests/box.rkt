#;
(
box.rkt line 17 col 1 - unbox - box
box.rkt line 18 col 1 - set-box! - box
box.rkt line 19 col 1 - unbox - box
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
