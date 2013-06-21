#;#;
#<<END
TR opt: define-pair.rkt 10:10 (car (quote (1 3))) -- pair
END
""

#lang typed/scheme
#:optimize

(define x (car '(1 3)))
