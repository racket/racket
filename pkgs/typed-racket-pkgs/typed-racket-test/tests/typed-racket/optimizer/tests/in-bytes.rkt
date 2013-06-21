#;#;
#<<END
TR opt: in-bytes.rkt 9:0 #%module-begin -- in-bytes
TR info: in-bytes.rkt 12:7 display -- hidden parameter
TR info: in-bytes.rkt 12:7 display -- hidden parameter

END
"495051"
#lang typed/scheme
#:optimize
(for: ((i : Integer #"123"))
      (display i))
