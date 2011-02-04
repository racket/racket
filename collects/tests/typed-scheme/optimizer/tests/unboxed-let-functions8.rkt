#;
(
unboxed-let-functions8.rkt line 15 col 67 - x - unbox float-complex
unboxed-let-functions8.rkt line 15 col 69 - 2.0+4.0i - unboxed literal
unboxed-let-functions8.rkt line 15 col 65 - + - unboxed binary float complex
unboxed-let-functions8.rkt line 15 col 64 - (#%app + x (quote 2.0+4.0i)) - unboxed float complex
3.0+6.0i
)

#lang typed/scheme
#:optimize



(letrec: ((f : (Float-Complex -> Float-Complex)     (lambda (x) (+ x 2.0+4.0i)))
          (g : (Float-Complex -> Float-Complex)     f)) ; f escapes! can't unbox its args
  (f 1.0+2.0i))
