#;
(
TR opt: fixnum-comparison.rkt 12:0 (< (vector-length (quote #(1 2 3))) (string-length "asdf")) -- binary fixnum
TR opt: fixnum-comparison.rkt 12:3 (vector-length (quote #(1 2 3))) -- vector-length
TR opt: fixnum-comparison.rkt 12:29 (string-length "asdf") -- string-length
#t
)

#lang typed/scheme
#:optimize

(< (vector-length '#(1 2 3)) (string-length "asdf"))
