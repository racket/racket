#;#;
#<<END
TR opt: pair-known-length-list.rkt 30:0 (car x) -- pair
TR opt: pair-known-length-list.rkt 31:0 (cdr x) -- pair
TR opt: pair-known-length-list.rkt 32:0 (car (cdr x)) -- pair
TR opt: pair-known-length-list.rkt 32:5 (cdr x) -- pair
TR opt: pair-known-length-list.rkt 33:0 (cdr (cdr x)) -- pair
TR opt: pair-known-length-list.rkt 33:5 (cdr x) -- pair
TR opt: pair-known-length-list.rkt 34:0 (car (cdr (cdr x))) -- pair
TR opt: pair-known-length-list.rkt 34:10 (cdr x) -- pair
TR opt: pair-known-length-list.rkt 34:5 (cdr (cdr x)) -- pair
TR opt: pair-known-length-list.rkt 35:0 (cdr (cdr (cdr x))) -- pair
TR opt: pair-known-length-list.rkt 35:10 (cdr x) -- pair
TR opt: pair-known-length-list.rkt 35:5 (cdr (cdr x)) -- pair
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

(: x (List Integer Integer Integer))
(define x (list 1 2 3))
(car x)
(cdr x)
(car (cdr x))
(cdr (cdr x))
(car (cdr (cdr x)))
(cdr (cdr (cdr x)))
