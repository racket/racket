#;
(
TR opt: in-bytes.rkt 7:0 #%module-begin -- in-bytes
TR info: in-bytes.rkt 10:7 display -- hidden parameter
TR info: in-bytes.rkt 10:7 display -- hidden parameter
495051)
#lang typed/scheme
#:optimize
(for: ((i : Integer #"123"))
      (display i))
