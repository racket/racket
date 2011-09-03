#;
(
TR opt: in-string.rkt 6:0 #%module-begin -- in-string
123)

#lang typed/scheme
#:optimize

(for: ((i : Char "123"))
      (display i))
