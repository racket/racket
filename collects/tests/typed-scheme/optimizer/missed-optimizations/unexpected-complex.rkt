#;
(
unexpected-complex.rkt 16:0 (#%app sqrt (quote 4)) -- unexpected complex value -- caused by: 16:1 sqrt
unexpected-complex.rkt 17:0 (#%app + (quote 1.2+3.4i) (quote 2.0)) -- unboxed float complex
unexpected-complex.rkt 17:1 + -- unboxed binary float complex
unexpected-complex.rkt 17:3 1.2+3.4i -- unboxed literal
unexpected-complex.rkt 17:12 (quote 2.0) -- float-coerce-expr in complex ops
2
3.2+3.4i
 )

#lang typed/racket

;; a Complex type is "unexpected" if it pops up in an expressions for which
;; all subexpressions have a Real type
(sqrt (ann 4 Integer))
(+ 1.2+3.4i 2.0) ; this one is expected, though
