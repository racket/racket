#;#;
#<<END
TR opt: float-complex-parts2.rkt 4:0 (real-part (+ 1.0+2.0i 2.0+4.0i)) -- complex accessor elimination
TR opt: float-complex-parts2.rkt 4:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 4:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 4:23 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 5:0 (unsafe-flreal-part (+ 1.0+2.0i 2.0+4.0i)) -- complex accessor elimination
TR opt: float-complex-parts2.rkt 5:20 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 5:23 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 5:32 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 6:0 (imag-part (+ 1.0+2.0i 2.0+4.0i)) -- complex accessor elimination
TR opt: float-complex-parts2.rkt 6:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 6:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 6:23 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 7:0 (unsafe-flimag-part (+ 1.0+2.0i 2.0+4.0i)) -- complex accessor elimination
TR opt: float-complex-parts2.rkt 7:20 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 7:23 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 7:32 2.0+4.0i -- unboxed literal
END
#<<END
3.0
3.0
6.0
6.0

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(require racket/unsafe/ops)

(real-part (+ 1.0+2.0i 2.0+4.0i))
(unsafe-flreal-part (+ 1.0+2.0i 2.0+4.0i))
(imag-part (+ 1.0+2.0i 2.0+4.0i))
(unsafe-flimag-part (+ 1.0+2.0i 2.0+4.0i))
