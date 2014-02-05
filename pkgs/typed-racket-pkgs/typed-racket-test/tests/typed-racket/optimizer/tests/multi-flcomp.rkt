#;#;
#<<END
TR missed opt: multi-flcomp.rkt 3:0 (<= 1.0 2.0 3) -- generic comparison -- caused by: 3:12 3
TR opt: multi-flcomp.rkt 2:0 (<= 1.0 2.0 3.0) -- multi float comp
TR opt: multi-flcomp.rkt 4:0 (<= 1.0 2.0 3.0 4.0) -- multi float comp
TR opt: multi-flcomp.rkt 5:0 (<= 1.0 2.0 3.0 (+ 2.0 2.0)) -- multi float comp
TR opt: multi-flcomp.rkt 5:16 (+ 2.0 2.0) -- binary float
TR opt: multi-flcomp.rkt 6:0 (<= 1.0 2.0 (+ 2.0 2.0) 3.0) -- multi float comp
TR opt: multi-flcomp.rkt 6:12 (+ 2.0 2.0) -- binary float
END
#<<END
#t
#t
#t
#t
#f

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

(<= 1.0 2.0 3.0)
(<= 1.0 2.0 3) ; unsafe, last one is not a float
(<= 1.0 2.0 3.0 4.0)
(<= 1.0 2.0 3.0 (+ 2.0 2.0))
(<= 1.0 2.0 (+ 2.0 2.0) 3.0)
