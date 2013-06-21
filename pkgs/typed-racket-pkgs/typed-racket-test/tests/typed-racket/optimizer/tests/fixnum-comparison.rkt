#;#;
#<<END
TR opt: fixnum-comparison.rkt 15:0 (< (vector-length (quote #(1 2 3))) (string-length "asdf")) -- binary fixnum comp
TR opt: fixnum-comparison.rkt 15:29 (string-length "asdf") -- string-length
TR opt: fixnum-comparison.rkt 15:3 (vector-length (quote #(1 2 3))) -- known-length vector-length
END
#<<END
#t

END

#lang typed/scheme
#:optimize

(< (vector-length '#(1 2 3)) (string-length "asdf"))
