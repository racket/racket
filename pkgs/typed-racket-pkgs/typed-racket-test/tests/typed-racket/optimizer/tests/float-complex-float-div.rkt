#;#;
#<<END
TR opt: float-complex-float-div.rkt 10:18 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 10:27 3.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 10:6 (/ 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 10:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 11:18 2.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 11:22 3.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 11:6 (/ 1.0+2.0i 2.0 3.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 11:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 12:13 2.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 12:17 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 12:6 (/ 1.0 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 12:9 1.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 3:51 (real-part x) -- complex accessor elimination
TR opt: float-complex-float-div.rkt 3:62 x -- unbox float-complex
TR opt: float-complex-float-div.rkt 4:51 (imag-part x) -- complex accessor elimination
TR opt: float-complex-float-div.rkt 4:62 x -- unbox float-complex
TR opt: float-complex-float-div.rkt 6:13 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 6:6 (/ 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 6:9 1.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 7:18 2.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 7:6 (/ 1.0+2.0i 2.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 7:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 8:13 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 8:22 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 8:6 (/ 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 8:9 1.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 9:18 2.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 9:22 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 9:6 (/ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 9:9 1.0+2.0i -- unboxed literal
END
#<<END
'("0.1000000000-0.2000000000" "0.50000000001.0000000000" "-0.0200000000-0.0266666667" "0.16666666670.0000000000" "0.16666666670.0000000000" "0.16666666670.3333333333" "0.0333333333-0.0666666667")

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

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
