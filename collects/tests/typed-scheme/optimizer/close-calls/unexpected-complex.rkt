#;
(
unexpected-complex.rkt 12:0 (#%app sqrt (quote 4)) -- unexpected complex value -- caused by: 12:1 sqrt
2
3.2+3.4i
 )

#lang typed/racket

;; a Complex type is "unexpected" if it pops up in an expressions for which
;; all subexpressions have a Real type
(sqrt (ann 4 Integer))
(+ 1.2+3.4i 2.0) ; this one is expected, though
