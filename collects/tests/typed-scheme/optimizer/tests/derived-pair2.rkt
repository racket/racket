#;
(
TR opt: derived-pair2.rkt 31:0 (caaar (cons (cons (cons 1 2) 3) 4)) -- derived pair
TR opt: derived-pair2.rkt 31:0 (caaar (cons (cons (cons 1 2) 3) 4)) -- pair
TR opt: derived-pair2.rkt 32:0 (caadr (cons 1 (cons (cons 2 3) 4))) -- derived pair
TR opt: derived-pair2.rkt 32:0 (caadr (cons 1 (cons (cons 2 3) 4))) -- pair
TR opt: derived-pair2.rkt 33:0 (cadar (cons (cons 1 (cons 2 3)) 4)) -- derived pair
TR opt: derived-pair2.rkt 33:0 (cadar (cons (cons 1 (cons 2 3)) 4)) -- pair
TR opt: derived-pair2.rkt 34:0 (caddr (cons 1 (cons 2 (cons 3 4)))) -- derived pair
TR opt: derived-pair2.rkt 34:0 (caddr (cons 1 (cons 2 (cons 3 4)))) -- pair
TR opt: derived-pair2.rkt 35:0 (cdaar (cons (cons (cons 1 2) 3) 4)) -- derived pair
TR opt: derived-pair2.rkt 35:0 (cdaar (cons (cons (cons 1 2) 3) 4)) -- pair
TR opt: derived-pair2.rkt 36:0 (cdadr (cons 1 (cons (cons 2 3) 4))) -- derived pair
TR opt: derived-pair2.rkt 36:0 (cdadr (cons 1 (cons (cons 2 3) 4))) -- pair
TR opt: derived-pair2.rkt 37:0 (cddar (cons (cons 1 (cons 2 3)) 4)) -- derived pair
TR opt: derived-pair2.rkt 37:0 (cddar (cons (cons 1 (cons 2 3)) 4)) -- pair
TR opt: derived-pair2.rkt 38:0 (cdddr (cons 1 (cons 2 (cons 3 4)))) -- derived pair
TR opt: derived-pair2.rkt 38:0 (cdddr (cons 1 (cons 2 (cons 3 4)))) -- pair
1
2
2
3
2
3
3
4
)

#lang typed/racket #:optimize

(caaar (cons (cons (cons 1 2) 3) 4))
(caadr (cons 1 (cons (cons 2 3) 4)))
(cadar (cons (cons 1 (cons 2 3)) 4))
(caddr (cons 1 (cons 2 (cons 3 4))))
(cdaar (cons (cons (cons 1 2) 3) 4))
(cdadr (cons 1 (cons (cons 2 3) 4)))
(cddar (cons (cons 1 (cons 2 3)) 4))
(cdddr (cons 1 (cons 2 (cons 3 4))))
