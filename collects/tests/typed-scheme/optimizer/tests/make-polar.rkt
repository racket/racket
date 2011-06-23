#;
(
TR opt: make-polar.rkt 28:0 (make-polar 1.0 1.0) -- make-polar
TR opt: make-polar.rkt 28:0 (make-polar 1.0 1.0) -- make-rectangular elimination
TR opt: make-polar.rkt 31:0 (let ((p (+ 1.0+2.0i (make-polar 2.0 4.0)))) (string-append (real->decimal-string (real-part p) 10) (real->decimal-string (imag-part p) 10))) -- unboxed let bindings
TR opt: make-polar.rkt 31:9 (+ 1.0+2.0i (make-polar 2.0 4.0)) -- unboxed binary float complex
TR opt: make-polar.rkt 31:12 1.0+2.0i -- unboxed literal
TR opt: make-polar.rkt 31:21 (make-polar 2.0 4.0) -- make-rectangular elimination
TR opt: make-polar.rkt 32:39 (real-part p) -- unboxed float complex
TR opt: make-polar.rkt 32:39 (real-part p) -- unboxed float complex->float
TR opt: make-polar.rkt 32:39 (real-part p) -- unboxed unary float complex
TR opt: make-polar.rkt 32:50 p -- leave var unboxed
TR opt: make-polar.rkt 32:50 p -- unbox float-complex
TR opt: make-polar.rkt 32:50 p -- unboxed complex variable
TR opt: make-polar.rkt 33:39 (imag-part p) -- unboxed float complex
TR opt: make-polar.rkt 33:50 p -- leave var unboxed
TR opt: make-polar.rkt 33:50 p -- unboxed complex variable
0.5403023058681398+0.8414709848078965i
"-0.30728724170.4863950094"
)

#lang typed/scheme
#:optimize



;; top level
(make-polar 1.0 1.0)

;; nested
(let ((p (+ 1.0+2.0i (make-polar 2.0 4.0))))
  (string-append (real->decimal-string (real-part p) 10)
                 (real->decimal-string (imag-part p) 10)))
