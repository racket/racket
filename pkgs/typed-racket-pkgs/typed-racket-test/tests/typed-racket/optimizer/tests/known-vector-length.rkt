#;#;
#<<END
TR opt: known-vector-length.rkt 14:0 (+ 2 (vector-length (ann (vector 1 2) (Vector Integer Integer)))) -- fixnum bounded expr
TR opt: known-vector-length.rkt 14:5 (vector-length (ann (vector 1 2) (Vector Integer Integer))) -- known-length vector-length
END
#<<END
4

END

#lang typed/scheme
#:optimize

(+ 2 (vector-length (ann (vector 1 2) (Vector Integer Integer))))
