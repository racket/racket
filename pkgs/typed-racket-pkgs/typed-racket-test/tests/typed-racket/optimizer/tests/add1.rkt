#;#;
#<<END
TR opt: add1.rkt 17:5 (add1 5) -- fixnum add1
TR opt: add1.rkt 18:5 (sub1 3) -- fixnum sub1
TR opt: add1.rkt 19:5 (add1 2.3) -- float add1
TR opt: add1.rkt 20:5 (sub1 2.25) -- float sub1
END
#<<END
6
2
3.3
1.25

END

#lang typed/racket #:optimize
(ann (add1 5) Fixnum)
(ann (sub1 3) Fixnum)
(ann (add1 2.3) Float)
(ann (sub1 2.25) Float)
