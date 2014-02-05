#;#;
#<<END
TR opt: derived-pair2.rkt 2:0 (caaar (cons (cons (cons 1 2) 3) 4)) -- pair
TR opt: derived-pair2.rkt 2:0 (caaar (cons (cons (cons 1 2) 3) 4)) -- pair
TR opt: derived-pair2.rkt 2:0 (caaar (cons (cons (cons 1 2) 3) 4)) -- pair
TR opt: derived-pair2.rkt 3:0 (caadr (cons 1 (cons (cons 2 3) 4))) -- pair
TR opt: derived-pair2.rkt 3:0 (caadr (cons 1 (cons (cons 2 3) 4))) -- pair
TR opt: derived-pair2.rkt 3:0 (caadr (cons 1 (cons (cons 2 3) 4))) -- pair
TR opt: derived-pair2.rkt 4:0 (cadar (cons (cons 1 (cons 2 3)) 4)) -- pair
TR opt: derived-pair2.rkt 4:0 (cadar (cons (cons 1 (cons 2 3)) 4)) -- pair
TR opt: derived-pair2.rkt 4:0 (cadar (cons (cons 1 (cons 2 3)) 4)) -- pair
TR opt: derived-pair2.rkt 5:0 (caddr (cons 1 (cons 2 (cons 3 4)))) -- pair
TR opt: derived-pair2.rkt 5:0 (caddr (cons 1 (cons 2 (cons 3 4)))) -- pair
TR opt: derived-pair2.rkt 5:0 (caddr (cons 1 (cons 2 (cons 3 4)))) -- pair
TR opt: derived-pair2.rkt 6:0 (cdaar (cons (cons (cons 1 2) 3) 4)) -- pair
TR opt: derived-pair2.rkt 6:0 (cdaar (cons (cons (cons 1 2) 3) 4)) -- pair
TR opt: derived-pair2.rkt 6:0 (cdaar (cons (cons (cons 1 2) 3) 4)) -- pair
TR opt: derived-pair2.rkt 7:0 (cdadr (cons 1 (cons (cons 2 3) 4))) -- pair
TR opt: derived-pair2.rkt 7:0 (cdadr (cons 1 (cons (cons 2 3) 4))) -- pair
TR opt: derived-pair2.rkt 7:0 (cdadr (cons 1 (cons (cons 2 3) 4))) -- pair
TR opt: derived-pair2.rkt 8:0 (cddar (cons (cons 1 (cons 2 3)) 4)) -- pair
TR opt: derived-pair2.rkt 8:0 (cddar (cons (cons 1 (cons 2 3)) 4)) -- pair
TR opt: derived-pair2.rkt 8:0 (cddar (cons (cons 1 (cons 2 3)) 4)) -- pair
TR opt: derived-pair2.rkt 9:0 (cdddr (cons 1 (cons 2 (cons 3 4)))) -- pair
TR opt: derived-pair2.rkt 9:0 (cdddr (cons 1 (cons 2 (cons 3 4)))) -- pair
TR opt: derived-pair2.rkt 9:0 (cdddr (cons 1 (cons 2 (cons 3 4)))) -- pair
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
#reader tests/typed-racket/optimizer/reset-port

(caaar (cons (cons (cons 1 2) 3) 4))
(caadr (cons 1 (cons (cons 2 3) 4)))
(cadar (cons (cons 1 (cons 2 3)) 4))
(caddr (cons 1 (cons 2 (cons 3 4))))
(cdaar (cons (cons (cons 1 2) 3) 4))
(cdadr (cons 1 (cons (cons 2 3) 4)))
(cddar (cons (cons 1 (cons 2 3)) 4))
(cdddr (cons 1 (cons 2 (cons 3 4))))
