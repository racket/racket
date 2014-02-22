#;#;
#<<END
TR opt: projections.rkt 10:0 (abs -0.0) -- unary float
TR opt: projections.rkt 11:0 (magnitude -0.0) -- unary float
TR opt: projections.rkt 2:0 (real-part 1) -- unary number
TR opt: projections.rkt 3:0 (imag-part 2) -- unary number
TR opt: projections.rkt 4:0 (abs 3) -- unary number
TR opt: projections.rkt 5:0 (magnitude 4) -- unary number
TR opt: projections.rkt 6:0 (angle 5) -- unary number
TR opt: projections.rkt 7:0 (abs 0.0) -- unary number
TR opt: projections.rkt 8:0 (magnitude 0.0) -- unary number
TR opt: projections.rkt 9:0 (angle 0.0) -- unary number
END
#<<END
1
0
3
4
0
0.0
0.0
0
0.0
0.0
3.141592653589793

END

#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

(real-part 1)
(imag-part 2)
(abs 3)
(magnitude 4)
(angle 5)
(abs 0.0)
(magnitude 0.0)
(angle 0.0)
(abs -0.0)
(magnitude -0.0)
(angle -0.0)
