#;
(
TR opt: in-vector.rkt 7:0 #%module-begin -- in-vector
TR info: in-vector.rkt 10:7 display -- hidden parameter
TR info: in-vector.rkt 10:7 display -- hidden parameter
123)
#lang typed/scheme
#:optimize
(for: ((i : Integer (vector 1 2 3)))
      (display i))
