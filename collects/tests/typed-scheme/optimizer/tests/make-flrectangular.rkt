#;
(
TR opt: make-flrectangular.rkt 12:1 make-rectangular -- binary float comp
TR opt: make-flrectangular.rkt 13:1 make-flrectangular -- binary float comp
1.0+2.2i
1.0+2.2i
)

#lang typed/scheme
#:optimize
(require racket/flonum)
(make-rectangular 1.0 2.2)
(make-flrectangular 1.0 2.2)
