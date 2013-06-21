#;#;
#<<END
TR opt: make-flrectangular.rkt 15:0 (make-rectangular 1.0 2.2) -- binary float comp
TR opt: make-flrectangular.rkt 16:0 (make-flrectangular 1.0 2.2) -- binary float comp
END
#<<END
1.0+2.2i
1.0+2.2i

END

#lang typed/scheme
#:optimize
(require racket/flonum)
(make-rectangular 1.0 2.2)
(make-flrectangular 1.0 2.2)
