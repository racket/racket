#;#;
#<<END
TR info: in-list.rkt 2:7 display -- hidden parameter
TR opt: in-list.rkt 1:20 (quote (1 2 3)) -- in-list
END
"123"
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
(for: ((i : Natural '(1 2 3)))
      (display i))
