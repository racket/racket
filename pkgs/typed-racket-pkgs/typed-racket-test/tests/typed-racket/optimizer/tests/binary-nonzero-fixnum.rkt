#;#;
#<<END
TR opt: binary-nonzero-fixnum.rkt 14:0 (modulo (vector-length (quote #(1 2 3))) 2) -- binary nonzero fixnum
TR opt: binary-nonzero-fixnum.rkt 14:8 (vector-length (quote #(1 2 3))) -- known-length vector-length
END
#<<END
1

END

#lang typed/scheme
#:optimize

(modulo (vector-length '#(1 2 3)) 2)
