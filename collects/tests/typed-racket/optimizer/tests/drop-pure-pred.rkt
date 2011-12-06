#;
(
TR opt: drop-pure-pred.rkt 13:6 (exact-integer? x) -- useless pure code
TR opt: drop-pure-pred.rkt 15:6 (list 2) -- dead else branch
'(1)
)

#lang typed/scheme
#:optimize

(let ()
  (define x 7)
  (if (exact-integer? x)
      (list 1)
      (list 2)))

