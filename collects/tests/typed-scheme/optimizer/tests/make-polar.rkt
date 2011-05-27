#;
(
make-polar.rkt 28:1 make-polar -- make-polar
make-polar.rkt 28:1 make-polar -- make-rectangular elimination
make-polar.rkt 31:0 (let-values (((p) (#%app + (quote 1.0+2.0i) (#%app make-polar (quote 2.0) (quote 4.0))))) (#%app string-append (#%app real->decimal-string (#%app real-part p) (quote 10)) (#%app real->decimal-string (#%app imag-part p) (quote 10)))) -- unboxed let bindings
make-polar.rkt 31:10 + -- unboxed binary float complex
make-polar.rkt 31:12 1.0+2.0i -- unboxed literal
make-polar.rkt 31:22 make-polar -- make-rectangular elimination
make-polar.rkt 32:39 (#%app real-part p) -- unboxed float complex->float
make-polar.rkt 32:40 real-part -- unboxed float complex
make-polar.rkt 32:40 real-part -- unboxed unary float complex
make-polar.rkt 32:50 p -- leave var unboxed
make-polar.rkt 32:50 p -- unbox float-complex
make-polar.rkt 32:50 p -- unboxed complex variable
make-polar.rkt 33:40 imag-part -- unboxed float complex
make-polar.rkt 33:50 p -- leave var unboxed
make-polar.rkt 33:50 p -- unboxed complex variable
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
