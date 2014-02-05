#;#;
#<<END
TR opt: apply-plus.rkt 2:0 (apply + (map add1 (list 1 2 3))) -- apply-map
TR opt: apply-plus.rkt 3:0 (apply * (map add1 (list 1 2 3))) -- apply-map
END
#<<END
9
24

END

#lang typed/racket
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(apply + (map add1 (list 1 2 3)))
(apply * (map add1 (list 1 2 3)))
