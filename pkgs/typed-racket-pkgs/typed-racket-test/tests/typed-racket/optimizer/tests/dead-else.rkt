#;#;
#<<END
TR info: dead-else.rkt 1:1 display -- hidden parameter
TR info: dead-else.rkt 4:1 display -- hidden parameter
TR opt: dead-else.rkt 2:13 (+ 2.0 3.0) -- binary float
TR opt: dead-else.rkt 3:13 (+ 4.0 5.0) -- dead else branch
TR opt: dead-else.rkt 5:13 (+ 2.0 3.0) -- binary float
TR opt: dead-else.rkt 6:13 (+ 4.0 5.0) -- dead else branch
END
"5.05.0"
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
(display (if (number? 3)
             (+ 2.0 3.0)
             (+ 4.0 5.0)))
(display (if #t
             (+ 2.0 3.0)
             (+ 4.0 5.0)))
