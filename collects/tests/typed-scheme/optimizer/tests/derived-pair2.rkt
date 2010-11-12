#;
(
#f line #f col #f - car - pair
#f line #f col #f - car - pair
#f line #f col #f - car - pair
derived-pair2.rkt line 47 col 0 - (#%app caaar (#%app cons (#%app cons (#%app cons (quote 1) (quote 2)) (quote 3)) (quote 4))) - derived pair
#f line #f col #f - cdr - pair
#f line #f col #f - car - pair
#f line #f col #f - car - pair
derived-pair2.rkt line 48 col 0 - (#%app caadr (#%app cons (quote 1) (#%app cons (#%app cons (quote 2) (quote 3)) (quote 4)))) - derived pair
#f line #f col #f - car - pair
#f line #f col #f - cdr - pair
#f line #f col #f - car - pair
derived-pair2.rkt line 49 col 0 - (#%app cadar (#%app cons (#%app cons (quote 1) (#%app cons (quote 2) (quote 3))) (quote 4))) - derived pair
#f line #f col #f - cdr - pair
#f line #f col #f - cdr - pair
#f line #f col #f - car - pair
derived-pair2.rkt line 50 col 0 - (#%app caddr (#%app cons (quote 1) (#%app cons (quote 2) (#%app cons (quote 3) (quote 4))))) - derived pair
#f line #f col #f - car - pair
#f line #f col #f - car - pair
#f line #f col #f - cdr - pair
derived-pair2.rkt line 51 col 0 - (#%app cdaar (#%app cons (#%app cons (#%app cons (quote 1) (quote 2)) (quote 3)) (quote 4))) - derived pair
#f line #f col #f - cdr - pair
#f line #f col #f - car - pair
#f line #f col #f - cdr - pair
derived-pair2.rkt line 52 col 0 - (#%app cdadr (#%app cons (quote 1) (#%app cons (#%app cons (quote 2) (quote 3)) (quote 4)))) - derived pair
#f line #f col #f - car - pair
#f line #f col #f - cdr - pair
#f line #f col #f - cdr - pair
derived-pair2.rkt line 53 col 0 - (#%app cddar (#%app cons (#%app cons (quote 1) (#%app cons (quote 2) (quote 3))) (quote 4))) - derived pair
#f line #f col #f - cdr - pair
#f line #f col #f - cdr - pair
#f line #f col #f - cdr - pair
derived-pair2.rkt line 54 col 0 - (#%app cdddr (#%app cons (quote 1) (#%app cons (quote 2) (#%app cons (quote 3) (quote 4))))) - derived pair
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
