#;
#<<END
TR opt: vector-ref2.rkt 11:0 (vector-ref (vector 1 2 3) 0) -- vector
1

END

#lang typed/scheme
#:optimize

(vector-ref (vector 1 2 3) 0)
