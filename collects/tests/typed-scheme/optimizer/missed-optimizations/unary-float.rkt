#;
(
TR missed opt: unary-float.rkt 14:0 (sin (ann 3.4 Real)) -- all args float-arg-expr, result not Float -- caused by: 14:10 3.4
TR missed opt: unary-float.rkt 15:0 (sin 3) -- all args float-arg-expr, result not Float -- caused by: 15:5 3
TR missed opt: unary-float.rkt 16:0 (abs (ann 3.4 Real)) -- all args float-arg-expr, result not Float -- caused by: 16:10 3.4
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

