#;#;
#<<END
TR missed opt: list.rkt 6:0 (rest (rest l)) -- car/cdr on a potentially empty list -- caused by: 6:6 (rest l)
TR opt: list.rkt 3:0 (first l) -- pair
TR opt: list.rkt 4:0 (rest l) -- pair
TR opt: list.rkt 5:0 (second l) -- pair
TR opt: list.rkt 5:0 (second l) -- pair
TR opt: list.rkt 6:6 (rest l) -- pair
TR opt: list.rkt 7:0 (third l) -- pair
TR opt: list.rkt 7:0 (third l) -- pair
TR opt: list.rkt 7:0 (third l) -- pair
TR opt: list.rkt 8:0 (fourth l) -- pair
TR opt: list.rkt 8:0 (fourth l) -- pair
TR opt: list.rkt 8:0 (fourth l) -- pair
TR opt: list.rkt 8:0 (fourth l) -- pair
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
#reader tests/typed-racket/optimizer/reset-port

(define: l : (List Integer Integer Integer Integer) '(1 2 3 4))
(first l)
(rest l)
(second l)
(rest (rest l))
(third l)
(fourth l)
