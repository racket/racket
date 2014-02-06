#;#;
#<<END
TR info: in-vector.rkt 2:7 display -- hidden parameter
TR opt: in-vector.rkt 1:20 (vector 1 2 3) -- in-vector
END
"123"
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
(for: ((i : Integer (vector 1 2 3)))
      (display i))
