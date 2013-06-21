#;
#<<END
TR opt: nested-pair2.rkt 12:0 (car (cdr (cons 3 (cons (cons 2 (quote ())) 1)))) -- pair
TR opt: nested-pair2.rkt 12:5 (cdr (cons 3 (cons (cons 2 (quote ())) 1))) -- pair
'(2)

END

#lang typed/scheme
#:optimize

(car (cdr (cons 3 (cons (cons 2 '()) 1))))
