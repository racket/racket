#;#;
#<<END
TR info: unary-float.rkt 20:0 (sin (ann 3.4 Real)) -- exact real arith
TR info: unary-float.rkt 21:0 (sin 3) -- exact real arith
TR info: unary-float.rkt 22:0 (abs (ann 3.4 Real)) -- exact real arith
TR missed opt: unary-float.rkt 20:0 (sin (ann 3.4 Real)) -- all args float-arg-expr, result not Float -- caused by: 20:10 3.4
TR missed opt: unary-float.rkt 21:0 (sin 3) -- all args float-arg-expr, result not Float -- caused by: 21:5 3
TR missed opt: unary-float.rkt 22:0 (abs (ann 3.4 Real)) -- all args float-arg-expr, result not Float -- caused by: 22:10 3.4
END
#<<END
-0.2555411020268312
0.1411200080598672
3.4
3

END

#lang typed/racket

(sin (ann 3.4 Real))
(sin 3)
(abs (ann 3.4 Real))
(abs (ann 3 Integer)) ; stays within Integer, should not be reported

