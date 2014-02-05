#;#;
#<<END
TR opt: unboxed-letrec-syntaxes+values.rkt 3:25 ((x) (+ 1.0+2.0i 2.0+4.0i)) -- unboxed let bindings
TR opt: unboxed-letrec-syntaxes+values.rkt 3:30 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-letrec-syntaxes+values.rkt 3:33 1.0+2.0i -- unboxed literal
TR opt: unboxed-letrec-syntaxes+values.rkt 3:42 2.0+4.0i -- unboxed literal
TR opt: unboxed-letrec-syntaxes+values.rkt 4:24 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-letrec-syntaxes+values.rkt 4:27 x -- leave var unboxed
TR opt: unboxed-letrec-syntaxes+values.rkt 4:29 2.0+4.0i -- unboxed literal
END
#<<END
5.0+10.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(letrec-syntaxes+values (((s) (syntax-rules () [(_ x) x])))
                        (((x) (+ 1.0+2.0i 2.0+4.0i)))
                        (+ x 2.0+4.0i))
