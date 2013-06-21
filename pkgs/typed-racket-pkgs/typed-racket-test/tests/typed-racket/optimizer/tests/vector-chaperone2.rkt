#;#;
#<<END
TR opt: vector-chaperone2.rkt 15:10 (vector-ref v i) -- vector partial bounds checking elimination
END
#<<END
'b

END
#lang typed/racket
(require/typed "vector-chaperone1.rkt"
               (v (Vectorof Symbol)))
(: i Fixnum)
(define i 0)
(: x Symbol)
(define x (vector-ref v i))
x
