#;#;
#<<END
TR opt: unboxed-exp.rkt 10:25 (exp 12.2-4.7i) -- unboxed unary float complex
TR opt: unboxed-exp.rkt 10:30 12.2-4.7i -- unboxed literal
TR opt: unboxed-exp.rkt 11:25 (exp -12.2+4.7i) -- unboxed unary float complex
TR opt: unboxed-exp.rkt 11:30 -12.2+4.7i -- unboxed literal
TR opt: unboxed-exp.rkt 4:39 (real-part p) -- complex accessor elimination
TR opt: unboxed-exp.rkt 4:50 p -- unbox float-complex
TR opt: unboxed-exp.rkt 5:39 (imag-part p) -- complex accessor elimination
TR opt: unboxed-exp.rkt 5:50 p -- unbox float-complex
TR opt: unboxed-exp.rkt 7:25 (exp 2.0+3.4i) -- unboxed unary float complex
TR opt: unboxed-exp.rkt 7:30 2.0+3.4i -- unboxed literal
TR opt: unboxed-exp.rkt 8:25 (exp 0.0+0.0i) -- unboxed unary float complex
TR opt: unboxed-exp.rkt 8:30 0.0+0.0i -- unboxed literal
TR opt: unboxed-exp.rkt 9:25 (exp -12.2-4.7i) -- unboxed unary float complex
TR opt: unboxed-exp.rkt 9:30 -12.2-4.7i -- unboxed literal
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
#reader tests/typed-racket/optimizer/reset-port

(: complex->decimal-string (Float-Complex -> String))
(define (complex->decimal-string p)
  (string-append (real->decimal-string (real-part p) 5)
                 (real->decimal-string (imag-part p) 5)))

(complex->decimal-string (exp 2.0+3.4i))
(complex->decimal-string (exp 0.0+0.0i))
(complex->decimal-string (exp -12.2-4.7i))
(complex->decimal-string (exp 12.2-4.7i))
(complex->decimal-string (exp -12.2+4.7i))
