#;#;
#<<END
TR opt: float-complex-parts2.rkt 45:0 (real-part (+ 1.0+2.0i 2.0+4.0i)) -- complex accessor elimination
TR opt: float-complex-parts2.rkt 45:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 45:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 45:23 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 46:0 (unsafe-flreal-part (+ 1.0+2.0i 2.0+4.0i)) -- complex accessor elimination
TR opt: float-complex-parts2.rkt 46:20 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 46:23 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 46:32 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 47:0 (imag-part (+ 1.0+2.0i 2.0+4.0i)) -- complex accessor elimination
TR opt: float-complex-parts2.rkt 47:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 47:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 47:23 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 48:0 (unsafe-flimag-part (+ 1.0+2.0i 2.0+4.0i)) -- complex accessor elimination
TR opt: float-complex-parts2.rkt 48:20 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 48:23 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 48:32 2.0+4.0i -- unboxed literal
END
#<<END
3.0
3.0
6.0
6.0

END













#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

(real-part (+ 1.0+2.0i 2.0+4.0i))
(unsafe-flreal-part (+ 1.0+2.0i 2.0+4.0i))
(imag-part (+ 1.0+2.0i 2.0+4.0i))
(unsafe-flimag-part (+ 1.0+2.0i 2.0+4.0i))
