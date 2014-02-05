#;#;
#<<END
TR info: dead-then.rkt 1:1 display -- hidden parameter
TR info: dead-then.rkt 4:1 display -- hidden parameter
TR opt: dead-then.rkt 2:13 (+ 2.0 3.0) -- dead then branch
TR opt: dead-then.rkt 3:13 (+ 4.0 5.0) -- binary float
TR opt: dead-then.rkt 5:13 (+ 2.0 3.0) -- dead then branch
TR opt: dead-then.rkt 6:13 (+ 4.0 5.0) -- binary float
END
"9.09.0"
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
(display (if (number? "eh")
             (+ 2.0 3.0)
             (+ 4.0 5.0)))
(display (if #f
             (+ 2.0 3.0)
             (+ 4.0 5.0)))
