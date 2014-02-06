#;#;
#<<END
TR info: flrandom.rkt 3:7 random -- hidden parameter (random)
TR info: flrandom.rkt 4:7 random -- hidden parameter (random)
TR opt: flrandom.rkt 3:6 (random) -- float 0-arg random
TR opt: flrandom.rkt 5:6 (random (current-pseudo-random-generator)) -- float random
TR opt: flrandom.rkt 6:6 (flrandom (current-pseudo-random-generator)) -- float random
END
""
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

(require racket/flonum)
(void (random)) ; yes
(void (random 2)) ; no
(void (random (current-pseudo-random-generator))) ; yes
(void (flrandom (current-pseudo-random-generator))) ; yes
