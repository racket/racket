#;#;
#<<END
TR opt: vector-chaperone2.rkt 6:10 (vector-ref v i) -- vector partial bounds checking elimination
END
#<<END
'b

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port
(require/typed "vector-chaperone1.rkt"
               (v (Vectorof Symbol)))
(: i Fixnum)
(define i 0)
(: x Symbol)
(define x (vector-ref v i))
x
