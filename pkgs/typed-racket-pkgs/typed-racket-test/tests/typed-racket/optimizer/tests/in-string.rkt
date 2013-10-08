#;#;
#<<END
TR info: in-string.rkt 11:7 display -- hidden parameter
TR info: in-string.rkt 11:7 display -- hidden parameter
TR opt: in-string.rkt 10:17 "123" -- in-string
END
"123"
#lang typed/scheme
#:optimize
(for: ((i : Char "123"))
      (display i))
