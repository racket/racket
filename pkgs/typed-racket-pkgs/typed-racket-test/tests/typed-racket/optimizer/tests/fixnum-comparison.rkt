#;#;
#<<END
TR opt: fixnum-comparison.rkt 2:0 (< (vector-length (quote #(1 2 3))) (string-length "asdf")) -- binary fixnum comp
TR opt: fixnum-comparison.rkt 2:29 (string-length "asdf") -- string-length
TR opt: fixnum-comparison.rkt 2:3 (vector-length (quote #(1 2 3))) -- known-length vector-length
END
#<<END
#t

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(< (vector-length '#(1 2 3)) (string-length "asdf"))
