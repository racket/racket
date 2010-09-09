#;
(
pair.rkt line 81 col 45 - car - pair
pair.rkt line 81 col 39 - car - pair
derived-pair.rkt line 27 col 0 - (#%app caar (#%app cons (#%app cons (quote 1) (quote 2)
) (quote 3))) - derived pair                                                           
pair.rkt line 83 col 45 - cdr - pair
pair.rkt line 83 col 39 - car - pair
derived-pair.rkt line 28 col 0 - (#%app cadr (#%app cons (quote 1) (#%app cons (quote 2)
 (quote 3)))) - derived pair                                                           
pair.rkt line 85 col 45 - car - pair
pair.rkt line 85 col 39 - cdr - pair
derived-pair.rkt line 29 col 0 - (#%app cdar (#%app cons (#%app cons (quote 1) (quote 2)
) (quote 3))) - derived pair                                                           
pair.rkt line 87 col 45 - cdr - pair
pair.rkt line 87 col 39 - cdr - pair
derived-pair.rkt line 30 col 0 - (#%app cddr (#%app cons (quote 1) (#%app cons (quote 2)
 (quote 3)))) - derived pair                                                           
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
