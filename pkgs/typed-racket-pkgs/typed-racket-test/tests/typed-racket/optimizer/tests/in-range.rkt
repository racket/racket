#;
#<<END
TR opt: in-range.rkt 8:0 #%module-begin -- in-range
TR info: in-range.rkt 11:3 display -- hidden parameter
TR info: in-range.rkt 11:3 display -- hidden parameter
0123
END
#lang typed/scheme
#:optimize
(for ([i 4])
  (display i))
