#;
(
unary-float.rkt 14:0 (#%app sin (quote 3.4)) -- unary, arg float-arg-expr, return type not Float
unary-float.rkt 15:0 (#%app sin (quote 3)) -- unary, arg float-arg-expr, return type not Float
unary-float.rkt 16:0 (#%app abs (quote 3.4)) -- unary, arg float-arg-expr, return type not Float
-0.2555411020268312
0.1411200080598672
3.4
3
)

#lang typed/racket

(sin (ann 3.4 Real))
(sin 3)
(abs (ann 3.4 Real))
(abs (ann 3 Integer)) ; stays within Integer, should not be reported

