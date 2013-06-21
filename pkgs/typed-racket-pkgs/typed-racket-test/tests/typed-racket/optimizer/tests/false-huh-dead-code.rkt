#;
#<<END
TR opt: false-huh-dead-code.rkt 11:16 1 -- dead then branch
TR opt: false-huh-dead-code.rkt 12:13 1 -- dead then branch
2
2

END

#lang typed/racket
(if (false? #t) 1 2)
(if (not #t) 1 2)
