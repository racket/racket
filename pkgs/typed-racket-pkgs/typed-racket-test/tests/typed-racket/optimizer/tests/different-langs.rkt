#;#;
#<<END
TR info: different-langs.rkt 3:0 (/ 1 2) -- possible exact real arith
END
#<<END
1/2

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
;; to see if the harness supports having the 2 versions of a test being
;; written in different languages
(/ 1 2)
