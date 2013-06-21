#;#;
#<<END
TR opt: apply-plus.rkt 15:0 (apply + (map add1 (list 1 2 3))) -- apply-map
TR opt: apply-plus.rkt 16:0 (apply * (map add1 (list 1 2 3))) -- apply-map
END
#<<END
9
24

END

#lang typed/racket
#:optimize

(apply + (map add1 (list 1 2 3)))
(apply * (map add1 (list 1 2 3)))
