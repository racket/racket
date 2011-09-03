#;
(
TR opt: in-bytes.rkt 6:0 #%module-begin -- in-bytes
495051)

#lang typed/scheme
#:optimize

(for: ((i : Integer #"123"))
      (display i))
