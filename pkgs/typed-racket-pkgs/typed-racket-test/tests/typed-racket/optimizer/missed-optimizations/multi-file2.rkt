#;#;
#<<END
TR info: multi-file1.rkt 13:2 (* x (ann 3 Integer)) -- exact real arith
TR missed opt: multi-file1.rkt 13:2 (* x (ann 3 Integer)) -- all args float-arg-expr, result not Float -- caused by: 13:12 3
TR opt: multi-file2.rkt 16:10 (+ 3 5) -- fixnum bounded expr
TR opt: multi-file2.rkt 16:3 (* 3.4 (+ 3 5)) -- binary float
END
#<<END
81.6

END

#lang typed/racket
(require "multi-file1.rkt")

(f (* 3.4 (+ 3 5)))
