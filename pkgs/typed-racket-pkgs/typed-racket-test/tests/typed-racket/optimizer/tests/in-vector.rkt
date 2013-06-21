#;#;
#<<END
TR info: in-vector.rkt 11:7 display -- hidden parameter
TR info: in-vector.rkt 11:7 display -- hidden parameter
TR opt: in-vector.rkt 8:0 #%module-begin -- in-vector
END
"123"
#lang typed/scheme
#:optimize
(for: ((i : Integer (vector 1 2 3)))
      (display i))
