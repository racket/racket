#;
(
TR opt: derived-pair3.rkt 55:0 (caaaar (cons (cons (cons (cons 1 2) 3) 4) 5)) -- derived pair
TR opt: derived-pair3.rkt 55:0 (caaaar (cons (cons (cons (cons 1 2) 3) 4) 5)) -- pair
TR opt: derived-pair3.rkt 56:0 (caaadr (cons 1 (cons (cons (cons 2 3) 4) 5))) -- derived pair
TR opt: derived-pair3.rkt 56:0 (caaadr (cons 1 (cons (cons (cons 2 3) 4) 5))) -- pair
TR opt: derived-pair3.rkt 57:0 (caadar (cons (cons 1 (cons (cons 2 3) 4)) 5)) -- derived pair
TR opt: derived-pair3.rkt 57:0 (caadar (cons (cons 1 (cons (cons 2 3) 4)) 5)) -- pair
TR opt: derived-pair3.rkt 58:0 (caaddr (cons 1 (cons 2 (cons (cons 3 4) 5)))) -- derived pair
TR opt: derived-pair3.rkt 58:0 (caaddr (cons 1 (cons 2 (cons (cons 3 4) 5)))) -- pair
TR opt: derived-pair3.rkt 59:0 (cadaar (cons (cons (cons 1 (cons 2 3)) 4) 5)) -- derived pair
TR opt: derived-pair3.rkt 59:0 (cadaar (cons (cons (cons 1 (cons 2 3)) 4) 5)) -- pair
TR opt: derived-pair3.rkt 60:0 (cadadr (cons 1 (cons (cons 2 (cons 3 4)) 5))) -- derived pair
TR opt: derived-pair3.rkt 60:0 (cadadr (cons 1 (cons (cons 2 (cons 3 4)) 5))) -- pair
TR opt: derived-pair3.rkt 61:0 (caddar (cons (cons 1 (cons 2 (cons 3 4))) 5)) -- derived pair
TR opt: derived-pair3.rkt 61:0 (caddar (cons (cons 1 (cons 2 (cons 3 4))) 5)) -- pair
TR opt: derived-pair3.rkt 62:0 (cadddr (cons 1 (cons 2 (cons 3 (cons 4 5))))) -- derived pair
TR opt: derived-pair3.rkt 62:0 (cadddr (cons 1 (cons 2 (cons 3 (cons 4 5))))) -- pair
TR opt: derived-pair3.rkt 63:0 (cdaaar (cons (cons (cons (cons 1 2) 3) 4) 5)) -- derived pair
TR opt: derived-pair3.rkt 63:0 (cdaaar (cons (cons (cons (cons 1 2) 3) 4) 5)) -- pair
TR opt: derived-pair3.rkt 64:0 (cdaadr (cons 1 (cons (cons (cons 2 3) 4) 5))) -- derived pair
TR opt: derived-pair3.rkt 64:0 (cdaadr (cons 1 (cons (cons (cons 2 3) 4) 5))) -- pair
TR opt: derived-pair3.rkt 65:0 (cdadar (cons (cons 1 (cons (cons 2 3) 4)) 5)) -- derived pair
TR opt: derived-pair3.rkt 65:0 (cdadar (cons (cons 1 (cons (cons 2 3) 4)) 5)) -- pair
TR opt: derived-pair3.rkt 66:0 (cdaddr (cons 1 (cons 2 (cons (cons 3 4) 5)))) -- derived pair
TR opt: derived-pair3.rkt 66:0 (cdaddr (cons 1 (cons 2 (cons (cons 3 4) 5)))) -- pair
TR opt: derived-pair3.rkt 67:0 (cddaar (cons (cons (cons 1 (cons 2 3)) 4) 5)) -- derived pair
TR opt: derived-pair3.rkt 67:0 (cddaar (cons (cons (cons 1 (cons 2 3)) 4) 5)) -- pair
TR opt: derived-pair3.rkt 68:0 (cddadr (cons 1 (cons (cons 2 (cons 3 4)) 5))) -- derived pair
TR opt: derived-pair3.rkt 68:0 (cddadr (cons 1 (cons (cons 2 (cons 3 4)) 5))) -- pair
TR opt: derived-pair3.rkt 69:0 (cdddar (cons (cons 1 (cons 2 (cons 3 4))) 5)) -- derived pair
TR opt: derived-pair3.rkt 69:0 (cdddar (cons (cons 1 (cons 2 (cons 3 4))) 5)) -- pair
TR opt: derived-pair3.rkt 70:0 (cddddr (cons 1 (cons 2 (cons 3 (cons 4 5))))) -- derived pair
TR opt: derived-pair3.rkt 70:0 (cddddr (cons 1 (cons 2 (cons 3 (cons 4 5))))) -- pair
1
2
2
3
2
3
3
4
2
3
3
4
3
4
4
5
)

#lang typed/racket #:optimize

(caaaar (cons (cons (cons (cons 1 2) 3) 4) 5))
(caaadr (cons 1 (cons (cons (cons 2 3) 4) 5)))
(caadar (cons (cons 1 (cons (cons 2 3) 4)) 5))
(caaddr (cons 1 (cons 2 (cons (cons 3 4) 5))))
(cadaar (cons (cons (cons 1 (cons 2 3)) 4) 5))
(cadadr (cons 1 (cons (cons 2 (cons 3 4)) 5)))
(caddar (cons (cons 1 (cons 2 (cons 3 4))) 5))
(cadddr (cons 1 (cons 2 (cons 3 (cons 4 5)))))
(cdaaar (cons (cons (cons (cons 1 2) 3) 4) 5))
(cdaadr (cons 1 (cons (cons (cons 2 3) 4) 5)))
(cdadar (cons (cons 1 (cons (cons 2 3) 4)) 5))
(cdaddr (cons 1 (cons 2 (cons (cons 3 4) 5))))
(cddaar (cons (cons (cons 1 (cons 2 3)) 4) 5))
(cddadr (cons 1 (cons (cons 2 (cons 3 4)) 5)))
(cdddar (cons (cons 1 (cons 2 (cons 3 4))) 5))
(cddddr (cons 1 (cons 2 (cons 3 (cons 4 5)))))
