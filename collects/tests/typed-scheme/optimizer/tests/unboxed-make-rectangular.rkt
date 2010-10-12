#;
(
unboxed-make-rectangular.rkt line 33 col 5 - x - unbox float-complex
unboxed-make-rectangular.rkt line 33 col 7 - 2.0+4.0i - unboxed literal
unboxed-make-rectangular.rkt line 33 col 3 - + - unboxed binary float complex
unboxed-make-rectangular.rkt line 33 col 2 - (#%app + x (quote 2.0+4.0i)) - unboxed float complex
unboxed-make-rectangular.rkt line 32 col 10 - make-rectangular - make-rectangular elimination
unboxed-make-rectangular.rkt line 32 col 0 - (let-values (((x) (#%app make-rectangular (quote 1.0) (quote 2.0)))) (#%app + x (quote 2.0+4.0i))) - unboxed let bindings
unboxed-make-rectangular.rkt line 33 col 5 - x - leave var unboxed
unboxed-make-rectangular.rkt line 33 col 7 - 2.0+4.0i - unboxed literal
unboxed-make-rectangular.rkt line 33 col 3 - + - unboxed binary float complex
unboxed-make-rectangular.rkt line 33 col 2 - (#%app + x (quote 2.0+4.0i)) - unboxed float complex
unboxed-make-rectangular.rkt line 35 col 5 - x - unbox float-complex
unboxed-make-rectangular.rkt line 35 col 7 - 2.0+4.0i - unboxed literal
unboxed-make-rectangular.rkt line 35 col 3 - + - unboxed binary float complex
unboxed-make-rectangular.rkt line 35 col 2 - (#%app + x (quote 2.0+4.0i)) - unboxed float complex
unboxed-make-rectangular.rkt line 34 col 10 - unsafe-make-flrectangular - make-rectangular elimination
unboxed-make-rectangular.rkt line 34 col 0 - (let-values (((x) (#%app unsafe-make-flrectangular (quote 1.0) (quote 2.0)))) (#%app + x (quote 2.0+4.0i))) - unboxed let bindings
unboxed-make-rectangular.rkt line 35 col 5 - x - leave var unboxed
unboxed-make-rectangular.rkt line 35 col 7 - 2.0+4.0i - unboxed literal
unboxed-make-rectangular.rkt line 35 col 3 - + - unboxed binary float complex
unboxed-make-rectangular.rkt line 35 col 2 - (#%app + x (quote 2.0+4.0i)) - unboxed float complex
3.0+6.0i
3.0+6.0i
)

#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

(let ((x (make-rectangular 1.0 2.0)))
  (+ x 2.0+4.0i))
(let ((x (unsafe-make-flrectangular 1.0 2.0)))
  (+ x 2.0+4.0i))
