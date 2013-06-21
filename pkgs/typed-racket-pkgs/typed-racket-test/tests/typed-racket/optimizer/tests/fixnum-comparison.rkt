#;#;
#<<END
TR opt: fixnum-comparison.rkt 16:3 (vector-length (quote #(1 2 3))) -- known-length vector-length
TR opt: fixnum-comparison.rkt 16:29 (string-length "asdf") -- string-length
TR opt: fixnum-comparison.rkt 16:0 (< (vector-length (quote #(1 2 3))) (string-length "asdf")) -- binary fixnum comp

END
#<<END
#t

END

#lang typed/scheme
#:optimize

(< (vector-length '#(1 2 3)) (string-length "asdf"))
