#;#;
#<<END
TR info: in-bytes.rkt 2:7 display -- hidden parameter
TR opt: in-bytes.rkt 1:20 #"123" -- in-bytes
END
"495051"
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
(for: ((i : Integer #"123"))
      (display i))
