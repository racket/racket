#;
(
TR opt: unboxed-letrec-syntaxes+values.rkt 18:0 (letrec-syntaxes+values (((s) (syntax-rules () ((_ x) x)))) (((x) (+ 1.0+2.0i 2.0+4.0i))) (+ x 2.0+4.0i)) -- unboxed let bindings
TR opt: unboxed-letrec-syntaxes+values.rkt 19:30 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-letrec-syntaxes+values.rkt 19:33 1.0+2.0i -- unboxed literal
TR opt: unboxed-letrec-syntaxes+values.rkt 19:42 2.0+4.0i -- unboxed literal
TR opt: unboxed-letrec-syntaxes+values.rkt 20:24 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-letrec-syntaxes+values.rkt 20:24 (+ x 2.0+4.0i) -- unboxed float complex
TR opt: unboxed-letrec-syntaxes+values.rkt 20:27 x -- leave var unboxed
TR opt: unboxed-letrec-syntaxes+values.rkt 20:27 x -- unbox float-complex
TR opt: unboxed-letrec-syntaxes+values.rkt 20:29 2.0+4.0i -- unboxed literal
5.0+10.0i
)

#lang typed/scheme
#:optimize

(letrec-syntaxes+values (((s) (syntax-rules () [(_ x) x])))
                        (((x) (+ 1.0+2.0i 2.0+4.0i)))
                        (+ x 2.0+4.0i))
