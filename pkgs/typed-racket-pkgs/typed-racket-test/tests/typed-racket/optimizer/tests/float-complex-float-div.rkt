#;#;
#<<END
TR opt: float-complex-float-div.rkt 43:51 (real-part x) -- complex accessor elimination
TR opt: float-complex-float-div.rkt 43:62 x -- unbox float-complex
TR opt: float-complex-float-div.rkt 44:51 (imag-part x) -- complex accessor elimination
TR opt: float-complex-float-div.rkt 44:62 x -- unbox float-complex
TR opt: float-complex-float-div.rkt 46:13 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 46:6 (/ 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 46:9 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 47:18 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 47:6 (/ 1.0+2.0i 2.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 47:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 48:13 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 48:22 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 48:6 (/ 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 48:9 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 49:18 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 49:22 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 49:6 (/ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 49:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 50:18 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 50:27 3.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 50:6 (/ 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 50:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 51:18 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 51:22 3.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 51:6 (/ 1.0+2.0i 2.0 3.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 51:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 52:13 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 52:17 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 52:6 (/ 1.0 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 52:9 1.0 -- float-arg-expr in complex ops
END
#<<END
'("0.1000000000-0.2000000000" "0.50000000001.0000000000" "-0.0200000000-0.0266666667" "0.16666666670.0000000000" "0.16666666670.0000000000" "0.16666666670.3333333333" "0.0333333333-0.0666666667")

END

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
