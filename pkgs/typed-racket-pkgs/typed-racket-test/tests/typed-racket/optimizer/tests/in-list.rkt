#;#;
#<<END
TR info: in-list.rkt 11:7 display -- hidden parameter
TR info: in-list.rkt 11:7 display -- hidden parameter
TR opt: in-list.rkt 10:20 (quote (1 2 3)) -- in-list
END
"123"
#lang typed/scheme
#:optimize
(for: ((i : Natural '(1 2 3)))
      (display i))
