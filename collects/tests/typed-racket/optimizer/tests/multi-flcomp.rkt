#;
(
TR opt: multi-flcomp.rkt 18:0 (<= 1.0 2.0 3.0) -- multi float comp
TR opt: multi-flcomp.rkt 20:0 (<= 1.0 2.0 3.0 4.0) -- multi float comp
TR opt: multi-flcomp.rkt 21:0 (<= 1.0 2.0 3.0 (+ 2.0 2.0)) -- multi float comp
TR opt: multi-flcomp.rkt 21:16 (+ 2.0 2.0) -- binary float
TR opt: multi-flcomp.rkt 22:0 (<= 1.0 2.0 (+ 2.0 2.0) 3.0) -- multi float comp
TR opt: multi-flcomp.rkt 22:12 (+ 2.0 2.0) -- binary float
#t
#t
#t
#t
#f
)

#lang typed/racket

(<= 1.0 2.0 3.0)
(<= 1.0 2.0 3) ; unsafe, last one is not a float
(<= 1.0 2.0 3.0 4.0)
(<= 1.0 2.0 3.0 (+ 2.0 2.0))
(<= 1.0 2.0 (+ 2.0 2.0) 3.0)
