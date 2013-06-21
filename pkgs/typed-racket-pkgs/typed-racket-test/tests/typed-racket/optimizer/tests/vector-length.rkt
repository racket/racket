#;#;
#<<END
TR opt: vector-length.rkt 15:0 (vector-length (vector 1 2 3)) -- known-length vector-length
TR opt: vector-length.rkt 16:0 (vector-length (ann (vector 4 5 6) (Vectorof Integer))) -- known-length vector-length
END
#<<END
3
3

END

#lang typed/scheme
#:optimize

(vector-length (vector 1 2 3))
(vector-length (ann (vector 4 5 6) (Vectorof Integer)))
