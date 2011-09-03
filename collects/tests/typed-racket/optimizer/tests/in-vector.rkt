#;
(
TR opt: in-vector.rkt 6:0 #%module-begin -- in-vector
123)

#lang typed/scheme
#:optimize

(for: ((i : Integer (vector 1 2 3)))
      (display i))
