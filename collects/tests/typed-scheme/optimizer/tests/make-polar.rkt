#;
(
make-polar.rkt line 28 col 1 - make-polar - make-rectangular elimination
make-polar.rkt line 28 col 1 - make-polar - make-polar
make-polar.rkt line 32 col 50 - p - unbox inexact-complex
make-polar.rkt line 32 col 40 - real-part - unboxed unary inexact complex
make-polar.rkt line 32 col 39 - (#%app real-part p) - unboxed inexact complex->float
make-polar.rkt line 31 col 12 - 1.0+2.0i - unboxed literal
make-polar.rkt line 31 col 22 - make-polar - make-rectangular elimination
make-polar.rkt line 31 col 10 - + - unboxed binary inexact complex
make-polar.rkt line 31 col 0 - (let-values (((p) (#%app + (quote 1.0+2.0i) (#%app make-polar (quote 2.0) (quote 4.0))))) (#%app string-append (#%app real->decimal-string (#%app real-part p) (quote 10)) (#%app real->decimal-string (#%app imag-part p) (quote 10)))) - unboxed let bindings
make-polar.rkt line 32 col 50 - p - unboxed complex variable
make-polar.rkt line 32 col 50 - p - leave var unboxed
make-polar.rkt line 32 col 40 - real-part - unboxed inexact complex
make-polar.rkt line 33 col 50 - p - unboxed complex variable
make-polar.rkt line 33 col 50 - p - leave var unboxed
make-polar.rkt line 33 col 40 - imag-part - unboxed inexact complex
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
