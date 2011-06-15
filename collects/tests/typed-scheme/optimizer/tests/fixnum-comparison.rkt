#;
(
TR opt: #f (no location) op -- string-length
TR opt: fixnum-comparison.rkt 12:1 < -- binary fixnum
TR opt: fixnum-comparison.rkt 12:4 vector-length -- vector-length
#t
)

#lang typed/scheme
#:optimize

(< (vector-length '#(1 2 3)) (string-length "asdf"))
