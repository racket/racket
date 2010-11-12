#;
(
fixnum-comparison.rkt line 12 col 4 - vector-length - vector-length
#f line #f col #f - op - string-length
fixnum-comparison.rkt line 12 col 1 - < - binary fixnum
#t
)

#lang typed/scheme
#:optimize

(< (vector-length '#(1 2 3)) (string-length "asdf"))
