#;#;
#<<END
TR missed opt: unexpected-complex.rkt 4:0 (sqrt (ann 4 Integer)) -- unexpected complex type
TR opt: unexpected-complex.rkt 5:0 (+ 1.2+3.4i 2.0) -- unboxed binary float complex
TR opt: unexpected-complex.rkt 5:12 2.0 -- float in complex ops
TR opt: unexpected-complex.rkt 5:3 1.2+3.4i -- unboxed literal
END
#<<END
2
3.2+3.4i

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

;; a Complex type is "unexpected" if it pops up in an expressions for which
;; all subexpressions have a Real type
(sqrt (ann 4 Integer))
(+ 1.2+3.4i 2.0) ; this one is expected, though
