#;#;
#<<END
TR info: in-string.rkt 2:7 display -- hidden parameter
TR opt: in-string.rkt 1:17 "123" -- in-string
END
"123"
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
(for: ((i : Char "123"))
      (display i))
