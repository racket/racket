#;
(
TR opt: float-complex-float-div.rkt 40:51 (real-part x) -- complex accessor elimination
TR opt: float-complex-float-div.rkt 40:62 x -- unbox float-complex
TR opt: float-complex-float-div.rkt 41:51 (imag-part x) -- complex accessor elimination
TR opt: float-complex-float-div.rkt 41:62 x -- unbox float-complex
TR opt: float-complex-float-div.rkt 43:6 (/ 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 43:9 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 43:13 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 44:6 (/ 1.0+2.0i 2.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 44:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 44:18 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 45:6 (/ 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 45:9 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 45:13 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 45:22 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 46:6 (/ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 46:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 46:18 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 46:22 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 47:6 (/ 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 47:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 47:18 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 47:27 3.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 48:6 (/ 1.0+2.0i 2.0 3.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 48:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 48:18 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 48:22 3.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 49:6 (/ 1.0 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 49:9 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 49:13 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 49:17 3.0+6.0i -- unboxed literal
'("0.1000000000-0.2000000000" "0.50000000001.0000000000" "-0.0200000000-0.0266666667" "0.16666666670.0000000000" "0.16666666670.0000000000" "0.16666666670.3333333333" "0.0333333333-0.0666666667")
)

#lang typed/scheme
#:optimize

(map (lambda: ((x : Float-Complex))
              (string-append (real->decimal-string (real-part x) 10)
                             (real->decimal-string (imag-part x) 10)))
     (list
      (/ 1.0 2.0+4.0i)
      (/ 1.0+2.0i 2.0)
      (/ 1.0 2.0+4.0i 3.0+6.0i)
      (/ 1.0+2.0i 2.0 3.0+6.0i)
      (/ 1.0+2.0i 2.0+4.0i 3.0)
      (/ 1.0+2.0i 2.0 3.0)
      (/ 1.0 2.0 3.0+6.0i)))
