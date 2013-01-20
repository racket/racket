#;
(
TR info: different-langs.rkt 10:0 (/ 1 2) -- exact real arith
1/2)

#lang typed/scheme
#:optimize
;; to see if the harness supports having the 2 versions of a test being
;; written in different languages
(/ 1 2)
