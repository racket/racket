#;
(
TR opt: unboxed-letrec-syntaxes+values.rkt 17:0 (letrec-syntaxes+values (((s) (syntax-rules () ((_ x) x)))) (((x) (+ 1.0+2.0i 2.0+4.0i))) (+ x 2.0+4.0i)) -- unboxed let bindings
TR opt: unboxed-letrec-syntaxes+values.rkt 18:30 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-letrec-syntaxes+values.rkt 18:33 1.0+2.0i -- unboxed literal
TR opt: unboxed-letrec-syntaxes+values.rkt 18:42 2.0+4.0i -- unboxed literal
TR opt: unboxed-letrec-syntaxes+values.rkt 19:24 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-letrec-syntaxes+values.rkt 19:27 x -- leave var unboxed
TR opt: unboxed-letrec-syntaxes+values.rkt 19:27 x -- unbox float-complex
TR opt: unboxed-letrec-syntaxes+values.rkt 19:29 2.0+4.0i -- unboxed literal
5.0+10.0i
)

#lang typed/scheme
#:optimize

(letrec-syntaxes+values (((s) (syntax-rules () [(_ x) x])))
                        (((x) (+ 1.0+2.0i 2.0+4.0i)))
                        (+ x 2.0+4.0i))
