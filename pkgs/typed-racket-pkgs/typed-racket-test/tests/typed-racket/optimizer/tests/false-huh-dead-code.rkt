#;#;
#<<END
TR opt: false-huh-dead-code.rkt 1:16 1 -- dead then branch
TR opt: false-huh-dead-code.rkt 2:13 1 -- dead then branch
END
#<<END
2
2

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port
(if (false? #t) 1 2)
(if (not #t) 1 2)
