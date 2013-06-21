#;#;
#<<END
TR opt: silent-dead-branch.rkt 13:0 (when (number? "1") 1) -- dead then branch
END
""

#lang typed/racket

;; then branch gets eliminated, which puts the test in a begin
;; however, doing that at the toplevel causes the result of (number? "1")
;; to be printed, which is bad.
;; this was PR 11928
(when (number? "1") 1)
