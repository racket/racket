#;
(
TR opt: binary-nonzero-fixnum.rkt 11:8 (vector-length (quote #(1 2 3))) -- vector-length
TR opt: binary-nonzero-fixnum.rkt 11:0 (modulo (vector-length (quote #(1 2 3))) 2) -- binary nonzero fixnum
1
)

#lang typed/scheme
#:optimize

(modulo (vector-length '#(1 2 3)) 2)
