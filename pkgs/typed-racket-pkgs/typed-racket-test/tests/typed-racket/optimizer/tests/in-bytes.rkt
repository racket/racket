#;
#<<END
TR opt: in-bytes.rkt 8:0 #%module-begin -- in-bytes
TR info: in-bytes.rkt 11:7 display -- hidden parameter
TR info: in-bytes.rkt 11:7 display -- hidden parameter
495051
END
#lang typed/scheme
#:optimize
(for: ((i : Integer #"123"))
      (display i))
