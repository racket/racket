#;
#<<END
TR opt: nested-pair1.rkt 12:0 (car (cdr (quote (1 2)))) -- pair
TR opt: nested-pair1.rkt 12:5 (cdr (quote (1 2))) -- pair
2

END

#lang typed/scheme
#:optimize

(car (cdr '(1 2)))
