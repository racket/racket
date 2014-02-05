#;#;
#<<END
TR opt: multi-file2.rkt 3:10 (+ 3 5) -- fixnum bounded expr
TR opt: multi-file2.rkt 3:3 (* 3.4 (+ 3 5)) -- binary float
END
#<<END
81.6

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port
(require "multi-file1.rkt")

(f (* 3.4 (+ 3 5)))
