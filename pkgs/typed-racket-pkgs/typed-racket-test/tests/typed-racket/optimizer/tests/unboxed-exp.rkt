#;#;
#<<END
TR opt: unboxed-exp.rkt 32:39 (real-part p) -- complex accessor elimination
TR opt: unboxed-exp.rkt 32:50 p -- unbox float-complex
TR opt: unboxed-exp.rkt 33:39 (imag-part p) -- complex accessor elimination
TR opt: unboxed-exp.rkt 33:50 p -- unbox float-complex
TR opt: unboxed-exp.rkt 35:25 (exp 2.0+3.4i) -- unboxed unary float complex
TR opt: unboxed-exp.rkt 35:30 2.0+3.4i -- unboxed literal
TR opt: unboxed-exp.rkt 36:25 (exp 0.0+0.0i) -- unboxed unary float complex
TR opt: unboxed-exp.rkt 36:30 0.0+0.0i -- unboxed literal
TR opt: unboxed-exp.rkt 37:25 (exp -12.2-4.7i) -- unboxed unary float complex
TR opt: unboxed-exp.rkt 37:30 -12.2-4.7i -- unboxed literal
TR opt: unboxed-exp.rkt 38:25 (exp 12.2-4.7i) -- unboxed unary float complex
TR opt: unboxed-exp.rkt 38:30 12.2-4.7i -- unboxed literal
TR opt: unboxed-exp.rkt 39:25 (exp -12.2+4.7i) -- unboxed unary float complex
TR opt: unboxed-exp.rkt 39:30 -12.2+4.7i -- unboxed literal
END
#<<END
"-7.14373-1.88821"
"1.000000.00000"
"-0.000000.00001"
"-2462.73189198773.89558"
"-0.00000-0.00001"

END

#lang typed/racket
#:optimize

(: complex->decimal-string (Float-Complex -> String))
(define (complex->decimal-string p)
  (string-append (real->decimal-string (real-part p) 5)
                 (real->decimal-string (imag-part p) 5)))

(complex->decimal-string (exp 2.0+3.4i))
(complex->decimal-string (exp 0.0+0.0i))
(complex->decimal-string (exp -12.2-4.7i))
(complex->decimal-string (exp 12.2-4.7i))
(complex->decimal-string (exp -12.2+4.7i))
