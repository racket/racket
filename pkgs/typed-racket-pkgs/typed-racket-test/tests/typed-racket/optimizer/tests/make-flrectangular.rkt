#;
#<<END
TR opt: make-flrectangular.rkt 13:0 (make-rectangular 1.0 2.2) -- binary float comp
TR opt: make-flrectangular.rkt 14:0 (make-flrectangular 1.0 2.2) -- binary float comp
1.0+2.2i
1.0+2.2i

END

#lang typed/scheme
#:optimize
(require racket/flonum)
(make-rectangular 1.0 2.2)
(make-flrectangular 1.0 2.2)
