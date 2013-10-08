#;#;
#<<END
TR info: in-bytes.rkt 11:7 display -- hidden parameter
TR info: in-bytes.rkt 11:7 display -- hidden parameter
TR opt: in-bytes.rkt 10:20 #"123" -- in-bytes
END
"495051"
#lang typed/scheme
#:optimize
(for: ((i : Integer #"123"))
      (display i))
