#;#;
#<<END
TR opt: nested-pair2.rkt 14:0 (car (cdr (cons 3 (cons (cons 2 (quote ())) 1)))) -- pair
TR opt: nested-pair2.rkt 14:5 (cdr (cons 3 (cons (cons 2 (quote ())) 1))) -- pair
END
#<<END
'(2)

END

#lang typed/scheme
#:optimize

(car (cdr (cons 3 (cons (cons 2 '()) 1))))
