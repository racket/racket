#;
#<<END
TR opt: simple-pair.rkt 11:0 (car (cons 1 2)) -- pair
1

END

#lang typed/scheme
#:optimize

(car (cons 1 2))
