#;
#<<END
TR opt: apply-plus.rkt 13:0 (apply + (map add1 (list 1 2 3))) -- apply-map
TR opt: apply-plus.rkt 14:0 (apply * (map add1 (list 1 2 3))) -- apply-map
9
24

END

#lang typed/racket
#:optimize

(apply + (map add1 (list 1 2 3)))
(apply * (map add1 (list 1 2 3)))
