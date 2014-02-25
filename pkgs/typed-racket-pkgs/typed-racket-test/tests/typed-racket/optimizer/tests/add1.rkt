#;#;
#<<END
TR missed opt: add1.rkt 2:0 (add1 (ann 5 Fixnum)) -- out of fixnum range
TR missed opt: add1.rkt 4:0 (sub1 (ann 3 Fixnum)) -- out of fixnum range
TR opt: add1.rkt 1:5 (add1 5) -- fixnum add1
TR opt: add1.rkt 3:5 (sub1 3) -- fixnum sub1
TR opt: add1.rkt 5:5 (add1 2.3) -- float add1
TR opt: add1.rkt 6:5 (sub1 2.25) -- float sub1
END
#<<END
6
6
2
2
3.3
1.25

END
#lang typed/racket #:optimize
#reader tests/typed-racket/optimizer/reset-port
(ann (add1 5) Fixnum)
(add1 (ann 5 Fixnum))
(ann (sub1 3) Fixnum)
(sub1 (ann 3 Fixnum))
(ann (add1 2.3) Float)
(ann (sub1 2.25) Float)
