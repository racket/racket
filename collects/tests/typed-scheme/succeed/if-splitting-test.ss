#lang typed-scheme
(define-type-alias number Number)
(define-type-alias boolean Boolean)
(define-type-alias symbol Symbol)
(define-type-alias top Any)
(define-type-alias list-of Listof)
(define: l : (list-of number)
  (cons 1 (cons 2 (cons 3 #{'() : (list-of number)}))))

(define: (g [x : number]) : number
  (cond [(memv x l) => car]
	[else 0]))

