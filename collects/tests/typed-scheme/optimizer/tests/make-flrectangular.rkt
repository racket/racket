#;
(
make-flrectangular.rkt line 12 col 1 - make-rectangular - binary float comp
make-flrectangular.rkt line 13 col 1 - make-flrectangular - binary float comp
1.0+2.2i
1.0+2.2i
)

#lang typed/scheme
#:optimize
(require racket/flonum)
(make-rectangular 1.0 2.2)
(make-flrectangular 1.0 2.2)
