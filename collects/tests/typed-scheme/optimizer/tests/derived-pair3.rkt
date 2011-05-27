#;
(
derived-pair3.rkt 69:0 (#%app caaaar (#%app cons (#%app cons (#%app cons (#%app cons (quote 1) (quote 2)) (quote 3)) (quote 4)) (quote 5))) -- derived pair
derived-pair3.rkt 69:0 car -- pair
derived-pair3.rkt 70:0 (#%app caaadr (#%app cons (quote 1) (#%app cons (#%app cons (#%app cons (quote 2) (quote 3)) (quote 4)) (quote 5)))) -- derived pair
derived-pair3.rkt 70:0 car -- pair
derived-pair3.rkt 70:0 cdr -- pair
derived-pair3.rkt 71:0 (#%app caadar (#%app cons (#%app cons (quote 1) (#%app cons (#%app cons (quote 2) (quote 3)) (quote 4))) (quote 5))) -- derived pair
derived-pair3.rkt 71:0 car -- pair
derived-pair3.rkt 71:0 cdr -- pair
derived-pair3.rkt 72:0 (#%app caaddr (#%app cons (quote 1) (#%app cons (quote 2) (#%app cons (#%app cons (quote 3) (quote 4)) (quote 5))))) -- derived pair
derived-pair3.rkt 72:0 car -- pair
derived-pair3.rkt 72:0 cdr -- pair
derived-pair3.rkt 73:0 (#%app cadaar (#%app cons (#%app cons (#%app cons (quote 1) (#%app cons (quote 2) (quote 3))) (quote 4)) (quote 5))) -- derived pair
derived-pair3.rkt 73:0 car -- pair
derived-pair3.rkt 73:0 cdr -- pair
derived-pair3.rkt 74:0 (#%app cadadr (#%app cons (quote 1) (#%app cons (#%app cons (quote 2) (#%app cons (quote 3) (quote 4))) (quote 5)))) -- derived pair
derived-pair3.rkt 74:0 car -- pair
derived-pair3.rkt 74:0 cdr -- pair
derived-pair3.rkt 75:0 (#%app caddar (#%app cons (#%app cons (quote 1) (#%app cons (quote 2) (#%app cons (quote 3) (quote 4)))) (quote 5))) -- derived pair
derived-pair3.rkt 75:0 car -- pair
derived-pair3.rkt 75:0 cdr -- pair
derived-pair3.rkt 76:0 (#%app cadddr (#%app cons (quote 1) (#%app cons (quote 2) (#%app cons (quote 3) (#%app cons (quote 4) (quote 5)))))) -- derived pair
derived-pair3.rkt 76:0 car -- pair
derived-pair3.rkt 76:0 cdr -- pair
derived-pair3.rkt 77:0 (#%app cdaaar (#%app cons (#%app cons (#%app cons (#%app cons (quote 1) (quote 2)) (quote 3)) (quote 4)) (quote 5))) -- derived pair
derived-pair3.rkt 77:0 car -- pair
derived-pair3.rkt 77:0 cdr -- pair
derived-pair3.rkt 78:0 (#%app cdaadr (#%app cons (quote 1) (#%app cons (#%app cons (#%app cons (quote 2) (quote 3)) (quote 4)) (quote 5)))) -- derived pair
derived-pair3.rkt 78:0 car -- pair
derived-pair3.rkt 78:0 cdr -- pair
derived-pair3.rkt 79:0 (#%app cdadar (#%app cons (#%app cons (quote 1) (#%app cons (#%app cons (quote 2) (quote 3)) (quote 4))) (quote 5))) -- derived pair
derived-pair3.rkt 79:0 car -- pair
derived-pair3.rkt 79:0 cdr -- pair
derived-pair3.rkt 80:0 (#%app cdaddr (#%app cons (quote 1) (#%app cons (quote 2) (#%app cons (#%app cons (quote 3) (quote 4)) (quote 5))))) -- derived pair
derived-pair3.rkt 80:0 car -- pair
derived-pair3.rkt 80:0 cdr -- pair
derived-pair3.rkt 81:0 (#%app cddaar (#%app cons (#%app cons (#%app cons (quote 1) (#%app cons (quote 2) (quote 3))) (quote 4)) (quote 5))) -- derived pair
derived-pair3.rkt 81:0 car -- pair
derived-pair3.rkt 81:0 cdr -- pair
derived-pair3.rkt 82:0 (#%app cddadr (#%app cons (quote 1) (#%app cons (#%app cons (quote 2) (#%app cons (quote 3) (quote 4))) (quote 5)))) -- derived pair
derived-pair3.rkt 82:0 car -- pair
derived-pair3.rkt 82:0 cdr -- pair
derived-pair3.rkt 83:0 (#%app cdddar (#%app cons (#%app cons (quote 1) (#%app cons (quote 2) (#%app cons (quote 3) (quote 4)))) (quote 5))) -- derived pair
derived-pair3.rkt 83:0 car -- pair
derived-pair3.rkt 83:0 cdr -- pair
derived-pair3.rkt 84:0 (#%app cddddr (#%app cons (quote 1) (#%app cons (quote 2) (#%app cons (quote 3) (#%app cons (quote 4) (quote 5)))))) -- derived pair
derived-pair3.rkt 84:0 cdr -- pair
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
