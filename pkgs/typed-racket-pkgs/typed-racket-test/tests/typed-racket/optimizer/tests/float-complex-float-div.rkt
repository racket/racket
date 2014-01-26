#;#;
#<<END
TR opt: float-complex-float-div.rkt 66:51 (real-part x) -- complex accessor elimination
TR opt: float-complex-float-div.rkt 66:62 x -- unbox float-complex
TR opt: float-complex-float-div.rkt 67:51 (imag-part x) -- complex accessor elimination
TR opt: float-complex-float-div.rkt 67:62 x -- unbox float-complex
TR opt: float-complex-float-div.rkt 69:13 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 69:6 (/ 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 69:9 1.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 70:18 2.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 70:6 (/ 1.0+2.0i 2.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 70:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 71:13 2.0+4.0i -- leave var unboxed
TR opt: float-complex-float-div.rkt 71:13 2.0+4.0i -- unboxed let bindings
TR opt: float-complex-float-div.rkt 71:13 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 71:22 3.0+6.0i -- leave var unboxed
TR opt: float-complex-float-div.rkt 71:22 3.0+6.0i -- unboxed let bindings
TR opt: float-complex-float-div.rkt 71:22 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 71:6 (/ 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 71:6 (/ 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 71:9 1.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 72:18 2.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 72:22 3.0+6.0i -- leave var unboxed
TR opt: float-complex-float-div.rkt 72:22 3.0+6.0i -- unboxed let bindings
TR opt: float-complex-float-div.rkt 72:22 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 72:6 (/ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 72:6 (/ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 72:9 1.0+2.0i -- leave var unboxed
TR opt: float-complex-float-div.rkt 72:9 1.0+2.0i -- unboxed let bindings
TR opt: float-complex-float-div.rkt 72:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 73:18 2.0+4.0i -- leave var unboxed
TR opt: float-complex-float-div.rkt 73:18 2.0+4.0i -- unboxed let bindings
TR opt: float-complex-float-div.rkt 73:18 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 73:27 3.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 73:6 (/ 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 73:6 (/ 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 73:9 1.0+2.0i -- leave var unboxed
TR opt: float-complex-float-div.rkt 73:9 1.0+2.0i -- unboxed let bindings
TR opt: float-complex-float-div.rkt 73:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 74:18 2.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 74:22 3.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 74:6 (/ 1.0+2.0i 2.0 3.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 74:6 (/ 1.0+2.0i 2.0 3.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 74:9 1.0+2.0i -- leave var unboxed
TR opt: float-complex-float-div.rkt 74:9 1.0+2.0i -- unboxed let bindings
TR opt: float-complex-float-div.rkt 74:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 75:13 2.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 75:17 3.0+6.0i -- leave var unboxed
TR opt: float-complex-float-div.rkt 75:17 3.0+6.0i -- unboxed let bindings
TR opt: float-complex-float-div.rkt 75:17 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 75:6 (/ 1.0 2.0 3.0+6.0i) -- binary float
TR opt: float-complex-float-div.rkt 75:6 (/ 1.0 2.0 3.0+6.0i) -- float in complex ops
TR opt: float-complex-float-div.rkt 75:6 (/ 1.0 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 75:9 1.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 75:9 1.0 -- float in complex ops
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
