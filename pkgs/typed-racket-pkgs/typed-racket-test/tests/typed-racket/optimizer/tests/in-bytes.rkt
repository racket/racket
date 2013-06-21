#;#;
#<<END
TR info: in-bytes.rkt 11:7 display -- hidden parameter
TR info: in-bytes.rkt 11:7 display -- hidden parameter
TR opt: in-bytes.rkt 8:0 #%module-begin -- in-bytes
END
"495051"
#lang typed/scheme
#:optimize
(for: ((i : Integer #"123"))
      (display i))
