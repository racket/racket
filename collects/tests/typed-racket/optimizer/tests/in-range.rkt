#;
(
TR opt: in-range.rkt 6:0 #%module-begin -- in-range
0123)

#lang typed/scheme
#:optimize

(for ([i 4])
  (display i))
