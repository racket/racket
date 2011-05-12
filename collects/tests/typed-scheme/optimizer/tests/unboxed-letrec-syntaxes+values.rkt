#;
(
unboxed-letrec-syntaxes+values.rkt 25:27 x -- unbox float-complex
unboxed-letrec-syntaxes+values.rkt 25:29 2.0+4.0i -- unboxed literal
unboxed-letrec-syntaxes+values.rkt 25:25 + -- unboxed binary float complex
unboxed-letrec-syntaxes+values.rkt 25:24 (#%app + x (quote 2.0+4.0i)) -- unboxed float complex
unboxed-letrec-syntaxes+values.rkt 24:33 1.0+2.0i -- unboxed literal
unboxed-letrec-syntaxes+values.rkt 24:42 2.0+4.0i -- unboxed literal
unboxed-letrec-syntaxes+values.rkt 24:31 + -- unboxed binary float complex
unboxed-letrec-syntaxes+values.rkt 23:0 (letrec-syntaxes+values (((s) (syntax-rules () ((_ x) x)))) (((x) (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)))) (#%app + x (quote 2.0+4.0i))) -- unboxed let bindings
unboxed-letrec-syntaxes+values.rkt 25:27 x -- leave var unboxed
unboxed-letrec-syntaxes+values.rkt 25:29 2.0+4.0i -- unboxed literal
unboxed-letrec-syntaxes+values.rkt 25:25 + -- unboxed binary float complex
unboxed-letrec-syntaxes+values.rkt 25:24 (#%app + x (quote 2.0+4.0i)) -- unboxed float complex
5.0+10.0i
)

#lang typed/scheme
#:optimize



(letrec-syntaxes+values (((s) (syntax-rules () [(_ x) x])))
                        (((x) (+ 1.0+2.0i 2.0+4.0i)))
                        (+ x 2.0+4.0i))
