#;
(
#f (no location) car -- pair
#f (no location) car -- pair
derived-pair.rkt 23:0 (#%app caar (#%app cons (#%app cons (quote 1) (quote 2)) (quote 3))) -- derived pair
#f (no location) cdr -- pair
#f (no location) car -- pair
derived-pair.rkt 24:0 (#%app cadr (#%app cons (quote 1) (#%app cons (quote 2) (quote 3)))) -- derived pair
#f (no location) car -- pair
#f (no location) cdr -- pair
derived-pair.rkt 25:0 (#%app cdar (#%app cons (#%app cons (quote 1) (quote 2)) (quote 3))) -- derived pair
#f (no location) cdr -- pair
#f (no location) cdr -- pair
derived-pair.rkt 26:0 (#%app cddr (#%app cons (quote 1) (#%app cons (quote 2) (quote 3)))) -- derived pair
1
2
2
3
)

#lang typed/racket #:optimize

(caar (cons (cons 1 2) 3))
(cadr (cons 1 (cons 2 3)))
(cdar (cons (cons 1 2) 3))
(cddr (cons 1 (cons 2 3)))
