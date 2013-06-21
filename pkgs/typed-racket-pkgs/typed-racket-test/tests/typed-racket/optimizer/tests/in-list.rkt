#;
#<<END
TR opt: in-list.rkt 8:0 #%module-begin -- in-list
TR info: in-list.rkt 11:7 display -- hidden parameter
TR info: in-list.rkt 11:7 display -- hidden parameter
123
END
#lang typed/scheme
#:optimize
(for: ((i : Natural '(1 2 3)))
      (display i))
