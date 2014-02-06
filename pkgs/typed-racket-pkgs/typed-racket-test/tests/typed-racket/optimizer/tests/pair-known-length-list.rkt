#;#;
#<<END
TR opt: pair-known-length-list.rkt 4:0 (car x) -- pair
TR opt: pair-known-length-list.rkt 5:0 (cdr x) -- pair
TR opt: pair-known-length-list.rkt 6:0 (car (cdr x)) -- pair
TR opt: pair-known-length-list.rkt 6:5 (cdr x) -- pair
TR opt: pair-known-length-list.rkt 7:0 (cdr (cdr x)) -- pair
TR opt: pair-known-length-list.rkt 7:5 (cdr x) -- pair
TR opt: pair-known-length-list.rkt 8:0 (car (cdr (cdr x))) -- pair
TR opt: pair-known-length-list.rkt 8:10 (cdr x) -- pair
TR opt: pair-known-length-list.rkt 8:5 (cdr (cdr x)) -- pair
TR opt: pair-known-length-list.rkt 9:0 (cdr (cdr (cdr x))) -- pair
TR opt: pair-known-length-list.rkt 9:10 (cdr x) -- pair
TR opt: pair-known-length-list.rkt 9:5 (cdr (cdr x)) -- pair
END
#<<END
1
'(2 3)
2
'(3)
3
'()

END
#lang typed/racket #:optimize
#reader tests/typed-racket/optimizer/reset-port

(: x (List Integer Integer Integer))
(define x (list 1 2 3))
(car x)
(cdr x)
(car (cdr x))
(cdr (cdr x))
(car (cdr (cdr x)))
(cdr (cdr (cdr x)))
