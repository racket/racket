#;#;
#<<END
TR opt: pr13788.rkt 12:0 (vector-length (vector 1 2 3)) -- known-length vector-length
END
#<<END
3

END

#lang typed/racket

(vector-length (vector 1 2 3)) ; should not print the vector
