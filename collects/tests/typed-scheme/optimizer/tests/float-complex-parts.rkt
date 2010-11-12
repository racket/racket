#;
(
float-complex-parts.rkt line 17 col 11 - 1.0+2.0i - unboxed literal
float-complex-parts.rkt line 17 col 1 - real-part - unboxed float complex
float-complex-parts.rkt line 18 col 11 - 1.0+2.0i - unboxed literal
float-complex-parts.rkt line 18 col 1 - imag-part - unboxed float complex
float-complex-parts.rkt line 19 col 11 - 1.0+2.0i - unboxed literal
float-complex-parts.rkt line 19 col 1 - real-part - unboxed float complex
1.0
2.0
1.0
)

#lang typed/scheme
#:optimize

(real-part 1.0+2.0i)
(imag-part 1+2.0i)
(real-part 1.0+2i)
