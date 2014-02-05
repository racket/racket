#;#;
#<<END
TR opt: nested-pair2.rkt 2:0 (car (cdr (cons 3 (cons (cons 2 (quote ())) 1)))) -- pair
TR opt: nested-pair2.rkt 2:5 (cdr (cons 3 (cons (cons 2 (quote ())) 1))) -- pair
END
#<<END
'(2)

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(car (cdr (cons 3 (cons (cons 2 '()) 1))))
