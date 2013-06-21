#;#;
#<<END
TR missed opt: list.rkt 33:0 (rest (rest l)) -- car/cdr on a potentially empty list -- caused by: 33:6 (rest l)
TR opt: list.rkt 30:0 (first l) -- pair
TR opt: list.rkt 31:0 (rest l) -- pair
TR opt: list.rkt 32:0 (second l) -- pair
TR opt: list.rkt 32:0 (second l) -- pair
TR opt: list.rkt 33:6 (rest l) -- pair
TR opt: list.rkt 34:0 (third l) -- pair
TR opt: list.rkt 34:0 (third l) -- pair
TR opt: list.rkt 34:0 (third l) -- pair
TR opt: list.rkt 35:0 (fourth l) -- pair
TR opt: list.rkt 35:0 (fourth l) -- pair
TR opt: list.rkt 35:0 (fourth l) -- pair
TR opt: list.rkt 35:0 (fourth l) -- pair
END
#<<END
1
'(2 3 4)
2
'(3 4)
3
4

END

#lang typed/racket

(define: l : (List Integer Integer Integer Integer) '(1 2 3 4))
(first l)
(rest l)
(second l)
(rest (rest l))
(third l)
(fourth l)
