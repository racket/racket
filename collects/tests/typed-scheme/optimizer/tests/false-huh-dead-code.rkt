#;
(
false-huh-dead-code.rkt line 10 col 16 - (quote 1) - dead then branch
false-huh-dead-code.rkt line 11 col 13 - (quote 1) - dead then branch
2
2
 )

#lang typed/racket
(if (false? #t) 1 2)
(if (not #t) 1 2)
