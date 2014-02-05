#;#;
#<<END
TR opt: pr13788.rkt 2:0 (vector-length (vector 1 2 3)) -- known-length vector-length
END
#<<END
3

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

(vector-length (vector 1 2 3)) ; should not print the vector
