#;
(
TR opt: unboxed-exp.rkt 23:5 2.0+3.4i -- unboxed literal
TR opt: unboxed-exp.rkt 23:0 (exp 2.0+3.4i) -- unboxed unary float complex
TR opt: unboxed-exp.rkt 24:5 0.0+0.0i -- unboxed literal
TR opt: unboxed-exp.rkt 24:0 (exp 0.0+0.0i) -- unboxed unary float complex
TR opt: unboxed-exp.rkt 25:5 -12.2-4.7i -- unboxed literal
TR opt: unboxed-exp.rkt 25:0 (exp -12.2-4.7i) -- unboxed unary float complex
TR opt: unboxed-exp.rkt 26:5 12.2-4.7i -- unboxed literal
TR opt: unboxed-exp.rkt 26:0 (exp 12.2-4.7i) -- unboxed unary float complex
TR opt: unboxed-exp.rkt 27:5 -12.2+4.7i -- unboxed literal
TR opt: unboxed-exp.rkt 27:0 (exp -12.2+4.7i) -- unboxed unary float complex
-7.143726081314396-1.8882075384588168i
1.0+0.0i
-6.232062158151458e-08+5.030069557694479e-06i
-2462.731893583747+198773.89557926523i
-6.232062158151458e-08-5.030069557694479e-06i
)

#lang typed/racket
#:optimize

(exp 2.0+3.4i)
(exp 0.0+0.0i)
(exp -12.2-4.7i)
(exp 12.2-4.7i)
(exp -12.2+4.7i)
