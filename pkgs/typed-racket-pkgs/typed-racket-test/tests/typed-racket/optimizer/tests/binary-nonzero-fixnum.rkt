#;
#<<END
TR opt: binary-nonzero-fixnum.rkt 12:8 (vector-length (quote #(1 2 3))) -- known-length vector-length
TR opt: binary-nonzero-fixnum.rkt 12:0 (modulo (vector-length (quote #(1 2 3))) 2) -- binary nonzero fixnum
1

END

#lang typed/scheme
#:optimize

(modulo (vector-length '#(1 2 3)) 2)
