#;#;
#<<END
TR opt: vector-ref2.rkt 14:0 (vector-ref (vector 1 2 3) 0) -- vector

END
#<<END
1

END

#lang typed/scheme
#:optimize

(vector-ref (vector 1 2 3) 0)
