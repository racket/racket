#;#;
#<<END
TR missed opt: multi-flcomp.rkt 23:0 (<= 1.0 2.0 3) -- generic comparison -- caused by: 23:12 3
TR opt: multi-flcomp.rkt 22:0 (<= 1.0 2.0 3.0) -- multi float comp
TR opt: multi-flcomp.rkt 24:0 (<= 1.0 2.0 3.0 4.0) -- multi float comp
TR opt: multi-flcomp.rkt 25:0 (<= 1.0 2.0 3.0 (+ 2.0 2.0)) -- multi float comp
TR opt: multi-flcomp.rkt 25:16 (+ 2.0 2.0) -- binary float
TR opt: multi-flcomp.rkt 26:0 (<= 1.0 2.0 (+ 2.0 2.0) 3.0) -- multi float comp
TR opt: multi-flcomp.rkt 26:12 (+ 2.0 2.0) -- binary float
END
#<<END
#t
#t
#t
#t
#f

END

#lang typed/racket

(<= 1.0 2.0 3.0)
(<= 1.0 2.0 3) ; unsafe, last one is not a float
(<= 1.0 2.0 3.0 4.0)
(<= 1.0 2.0 3.0 (+ 2.0 2.0))
(<= 1.0 2.0 (+ 2.0 2.0) 3.0)
