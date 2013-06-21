#;#;
#<<END
TR info: different-langs.rkt 14:0 (/ 1 2) -- exact real arith
END
#<<END
1/2

END

#lang typed/scheme
#:optimize
;; to see if the harness supports having the 2 versions of a test being
;; written in different languages
(/ 1 2)
