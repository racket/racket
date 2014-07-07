#;#;
#<<END
TR opt: both-if-branches-dead.rkt 4:6 12 -- dead then branch
TR opt: both-if-branches-dead.rkt 5:6 (* 3 4) -- dead else branch
END
""
#lang typed/racket/base
#reader tests/typed-racket/optimizer/reset-port
;; Test that code where neither branch is taken works
(let/ec: k : Any
  (if (k (void))
      12
      (* 3 4)))
