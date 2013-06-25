#;
(
TR opt: false-huh-dead-code.rkt 10:16 1 -- dead then branch
TR opt: false-huh-dead-code.rkt 11:13 1 -- dead then branch
2
2
)

#lang typed/racket
(if (false? #t) 1 2)
(if (not #t) 1 2)
