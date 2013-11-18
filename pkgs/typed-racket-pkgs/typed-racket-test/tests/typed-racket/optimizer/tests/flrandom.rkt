#;#;
#<<END
TR info: flrandom.rkt 14:7 random -- hidden parameter (random)
TR info: flrandom.rkt 15:7 random -- hidden parameter (random)
TR opt: flrandom.rkt 14:6 (random) -- float 0-arg random
TR opt: flrandom.rkt 16:6 (random (current-pseudo-random-generator)) -- float random
TR opt: flrandom.rkt 17:6 (flrandom (current-pseudo-random-generator)) -- float random
END
""

#lang typed/racket

(require racket/flonum)
(void (random)) ; yes
(void (random 2)) ; no
(void (random (current-pseudo-random-generator))) ; yes
(void (flrandom (current-pseudo-random-generator))) ; yes
