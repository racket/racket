#;#;
#<<END
TR opt: nested-pair1.rkt 14:0 (car (cdr (quote (1 2)))) -- pair
TR opt: nested-pair1.rkt 14:5 (cdr (quote (1 2))) -- pair
END
#<<END
2

END

#lang typed/scheme
#:optimize

(car (cdr '(1 2)))
