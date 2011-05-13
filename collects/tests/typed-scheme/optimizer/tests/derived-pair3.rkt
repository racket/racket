#;
(
#f (no location) car -- pair
#f (no location) car -- pair
#f (no location) car -- pair
#f (no location) car -- pair
derived-pair3.rkt 103:0 (#%app caaaar (#%app cons (#%app cons (#%app cons (#%app cons (quote 1) (quote 2)) (quote 3)) (quote 4)) (quote 5))) -- derived pair
#f (no location) car -- pair
#f (no location) car -- pair
#f (no location) car -- pair
#f (no location) cdr -- pair
derived-pair3.rkt 104:0 (#%app caaadr (#%app cons (quote 1) (#%app cons (#%app cons (#%app cons (quote 2) (quote 3)) (quote 4)) (quote 5)))) -- derived pair
#f (no location) car -- pair
#f (no location) car -- pair
#f (no location) cdr -- pair
#f (no location) car -- pair
derived-pair3.rkt 105:0 (#%app caadar (#%app cons (#%app cons (quote 1) (#%app cons (#%app cons (quote 2) (quote 3)) (quote 4))) (quote 5))) -- derived pair
#f (no location) car -- pair
#f (no location) car -- pair
#f (no location) cdr -- pair
#f (no location) cdr -- pair
derived-pair3.rkt 106:0 (#%app caaddr (#%app cons (quote 1) (#%app cons (quote 2) (#%app cons (#%app cons (quote 3) (quote 4)) (quote 5))))) -- derived pair
#f (no location) car -- pair
#f (no location) cdr -- pair
#f (no location) car -- pair
#f (no location) car -- pair
derived-pair3.rkt 107:0 (#%app cadaar (#%app cons (#%app cons (#%app cons (quote 1) (#%app cons (quote 2) (quote 3))) (quote 4)) (quote 5))) -- derived pair
#f (no location) car -- pair
#f (no location) cdr -- pair
#f (no location) car -- pair
#f (no location) cdr -- pair
derived-pair3.rkt 108:0 (#%app cadadr (#%app cons (quote 1) (#%app cons (#%app cons (quote 2) (#%app cons (quote 3) (quote 4))) (quote 5)))) -- derived pair
#f (no location) car -- pair
#f (no location) cdr -- pair
#f (no location) cdr -- pair
#f (no location) car -- pair
derived-pair3.rkt 109:0 (#%app caddar (#%app cons (#%app cons (quote 1) (#%app cons (quote 2) (#%app cons (quote 3) (quote 4)))) (quote 5))) -- derived pair
#f (no location) car -- pair
#f (no location) cdr -- pair
#f (no location) cdr -- pair
#f (no location) cdr -- pair
derived-pair3.rkt 110:0 (#%app cadddr (#%app cons (quote 1) (#%app cons (quote 2) (#%app cons (quote 3) (#%app cons (quote 4) (quote 5)))))) -- derived pair
#f (no location) cdr -- pair
#f (no location) car -- pair
#f (no location) car -- pair
#f (no location) car -- pair
derived-pair3.rkt 111:0 (#%app cdaaar (#%app cons (#%app cons (#%app cons (#%app cons (quote 1) (quote 2)) (quote 3)) (quote 4)) (quote 5))) -- derived pair
#f (no location) cdr -- pair
#f (no location) car -- pair
#f (no location) car -- pair
#f (no location) cdr -- pair
derived-pair3.rkt 112:0 (#%app cdaadr (#%app cons (quote 1) (#%app cons (#%app cons (#%app cons (quote 2) (quote 3)) (quote 4)) (quote 5)))) -- derived pair
#f (no location) cdr -- pair
#f (no location) car -- pair
#f (no location) cdr -- pair
#f (no location) car -- pair
derived-pair3.rkt 113:0 (#%app cdadar (#%app cons (#%app cons (quote 1) (#%app cons (#%app cons (quote 2) (quote 3)) (quote 4))) (quote 5))) -- derived pair
#f (no location) cdr -- pair
#f (no location) car -- pair
#f (no location) cdr -- pair
#f (no location) cdr -- pair
derived-pair3.rkt 114:0 (#%app cdaddr (#%app cons (quote 1) (#%app cons (quote 2) (#%app cons (#%app cons (quote 3) (quote 4)) (quote 5))))) -- derived pair
#f (no location) cdr -- pair
#f (no location) cdr -- pair
#f (no location) car -- pair
#f (no location) car -- pair
derived-pair3.rkt 115:0 (#%app cddaar (#%app cons (#%app cons (#%app cons (quote 1) (#%app cons (quote 2) (quote 3))) (quote 4)) (quote 5))) -- derived pair
#f (no location) cdr -- pair
#f (no location) cdr -- pair
#f (no location) car -- pair
#f (no location) cdr -- pair
derived-pair3.rkt 116:0 (#%app cddadr (#%app cons (quote 1) (#%app cons (#%app cons (quote 2) (#%app cons (quote 3) (quote 4))) (quote 5)))) -- derived pair
#f (no location) cdr -- pair
#f (no location) cdr -- pair
#f (no location) cdr -- pair
#f (no location) car -- pair
derived-pair3.rkt 117:0 (#%app cdddar (#%app cons (#%app cons (quote 1) (#%app cons (quote 2) (#%app cons (quote 3) (quote 4)))) (quote 5))) -- derived pair
#f (no location) cdr -- pair
#f (no location) cdr -- pair
#f (no location) cdr -- pair
#f (no location) cdr -- pair
derived-pair3.rkt 118:0 (#%app cddddr (#%app cons (quote 1) (#%app cons (quote 2) (#%app cons (quote 3) (#%app cons (quote 4) (quote 5)))))) -- derived pair
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
