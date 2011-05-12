#;
(
fixnum-comparison.rkt 12:4 vector-length -- vector-length
#f (no location) op -- string-length
fixnum-comparison.rkt 12:1 < -- binary fixnum
#t
)

#lang typed/scheme
#:optimize

(< (vector-length '#(1 2 3)) (string-length "asdf"))
