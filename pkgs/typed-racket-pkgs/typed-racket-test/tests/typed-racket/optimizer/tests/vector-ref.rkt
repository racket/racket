#;
#<<END
TR opt: vector-ref.rkt 11:0 (vector-ref (ann (vector 1 2) (Vector Integer Integer)) 0) -- vector
1

END

#lang typed/scheme
#:optimize

(vector-ref (ann (vector 1 2) (Vector Integer Integer)) 0)
