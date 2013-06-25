#;
(
TR opt: in-string.rkt 7:0 #%module-begin -- in-string
TR info: in-string.rkt 10:7 display -- hidden parameter
TR info: in-string.rkt 10:7 display -- hidden parameter
123)
#lang typed/scheme
#:optimize
(for: ((i : Char "123"))
      (display i))
