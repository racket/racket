#;#;
#<<END
TR opt: binary-nonzero-fixnum.rkt 2:0 (modulo (vector-length (quote #(1 2 3))) 2) -- binary nonzero fixnum
TR opt: binary-nonzero-fixnum.rkt 2:8 (vector-length (quote #(1 2 3))) -- known-length vector-length
END
#<<END
1

END

#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(modulo (vector-length '#(1 2 3)) 2)
