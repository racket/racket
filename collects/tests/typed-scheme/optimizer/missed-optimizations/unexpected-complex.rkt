#;
(
TR missed opt: unexpected-complex.rkt 16:0 (sqrt (ann 4 Integer)) -- unexpected complex type
TR opt: unexpected-complex.rkt 17:0 (+ 1.2+3.4i 2.0) -- unboxed float complex
TR opt: unexpected-complex.rkt 17:1 + -- unboxed binary float complex
TR opt: unexpected-complex.rkt 17:3 1.2+3.4i -- unboxed literal
TR opt: unexpected-complex.rkt 17:12 2.0 -- float-arg-expr in complex ops
2
3.2+3.4i
)

#lang typed/racket

;; a Complex type is "unexpected" if it pops up in an expressions for which
;; all subexpressions have a Real type
(sqrt (ann 4 Integer))
(+ 1.2+3.4i 2.0) ; this one is expected, though
