#;#;
#<<END
TR opt: in-string.rkt 8:0 #%module-begin -- in-string
TR info: in-string.rkt 11:7 display -- hidden parameter
TR info: in-string.rkt 11:7 display -- hidden parameter
END
"123"
#lang typed/scheme
#:optimize
(for: ((i : Char "123"))
      (display i))
