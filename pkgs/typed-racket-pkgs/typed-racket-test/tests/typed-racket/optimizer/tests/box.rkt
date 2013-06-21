#;#;
#<<END
TR opt: box.rkt 20:0 (unbox x) -- box
TR opt: box.rkt 21:0 (set-box! x 2) -- box
TR opt: box.rkt 22:0 (unbox x) -- box
END
#<<END
1
2

END

#lang typed/scheme
#:optimize



(: x (Boxof Integer))
(define x (box 1))
(unbox x)
(set-box! x 2)
(unbox x)
