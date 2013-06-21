#;#;
#<<END
TR opt: derived-pair2.rkt 42:0 (caaar (cons (cons (cons 1 2) 3) 4)) -- pair
TR opt: derived-pair2.rkt 42:0 (caaar (cons (cons (cons 1 2) 3) 4)) -- pair
TR opt: derived-pair2.rkt 42:0 (caaar (cons (cons (cons 1 2) 3) 4)) -- pair
TR opt: derived-pair2.rkt 43:0 (caadr (cons 1 (cons (cons 2 3) 4))) -- pair
TR opt: derived-pair2.rkt 43:0 (caadr (cons 1 (cons (cons 2 3) 4))) -- pair
TR opt: derived-pair2.rkt 43:0 (caadr (cons 1 (cons (cons 2 3) 4))) -- pair
TR opt: derived-pair2.rkt 44:0 (cadar (cons (cons 1 (cons 2 3)) 4)) -- pair
TR opt: derived-pair2.rkt 44:0 (cadar (cons (cons 1 (cons 2 3)) 4)) -- pair
TR opt: derived-pair2.rkt 44:0 (cadar (cons (cons 1 (cons 2 3)) 4)) -- pair
TR opt: derived-pair2.rkt 45:0 (caddr (cons 1 (cons 2 (cons 3 4)))) -- pair
TR opt: derived-pair2.rkt 45:0 (caddr (cons 1 (cons 2 (cons 3 4)))) -- pair
TR opt: derived-pair2.rkt 45:0 (caddr (cons 1 (cons 2 (cons 3 4)))) -- pair
TR opt: derived-pair2.rkt 46:0 (cdaar (cons (cons (cons 1 2) 3) 4)) -- pair
TR opt: derived-pair2.rkt 46:0 (cdaar (cons (cons (cons 1 2) 3) 4)) -- pair
TR opt: derived-pair2.rkt 46:0 (cdaar (cons (cons (cons 1 2) 3) 4)) -- pair
TR opt: derived-pair2.rkt 47:0 (cdadr (cons 1 (cons (cons 2 3) 4))) -- pair
TR opt: derived-pair2.rkt 47:0 (cdadr (cons 1 (cons (cons 2 3) 4))) -- pair
TR opt: derived-pair2.rkt 47:0 (cdadr (cons 1 (cons (cons 2 3) 4))) -- pair
TR opt: derived-pair2.rkt 48:0 (cddar (cons (cons 1 (cons 2 3)) 4)) -- pair
TR opt: derived-pair2.rkt 48:0 (cddar (cons (cons 1 (cons 2 3)) 4)) -- pair
TR opt: derived-pair2.rkt 48:0 (cddar (cons (cons 1 (cons 2 3)) 4)) -- pair
TR opt: derived-pair2.rkt 49:0 (cdddr (cons 1 (cons 2 (cons 3 4)))) -- pair
TR opt: derived-pair2.rkt 49:0 (cdddr (cons 1 (cons 2 (cons 3 4)))) -- pair
TR opt: derived-pair2.rkt 49:0 (cdddr (cons 1 (cons 2 (cons 3 4)))) -- pair
END
#<<END
1
2
2
3
2
3
3
4

END

#lang typed/racket #:optimize

(caaar (cons (cons (cons 1 2) 3) 4))
(caadr (cons 1 (cons (cons 2 3) 4)))
(cadar (cons (cons 1 (cons 2 3)) 4))
(caddr (cons 1 (cons 2 (cons 3 4))))
(cdaar (cons (cons (cons 1 2) 3) 4))
(cdadr (cons 1 (cons (cons 2 3) 4)))
(cddar (cons (cons 1 (cons 2 3)) 4))
(cdddr (cons 1 (cons 2 (cons 3 4))))
