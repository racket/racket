#;#;
#<<END
TR opt: simple-pair.rkt 13:0 (car (cons 1 2)) -- pair
END
#<<END
1

END

#lang typed/scheme
#:optimize

(car (cons 1 2))
