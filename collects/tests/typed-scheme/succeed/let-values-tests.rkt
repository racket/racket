#lang typed-scheme
(define-type-alias number Number)
(define-type-alias boolean Boolean)
(define-type-alias symbol Symbol)
(define-type-alias top Any)
(define-type-alias list-of Listof)
(let-values ([(#{x : number} #{y : number}) (values 3 4)]
	     [(#{z : number}) (values 3)]
	     #;[(#{fact : (number -> number)})
	     (lambda: ([x : number])
	     (if (zero? x) 1 (* x (fact (- x 1)))))]
	     #;[(#{z : number}) (- x y)])
	    (+ x y))

(letrec-values ([(#{x : number} #{y : number}) (values 3 4)])
	       (+ x y))
(letrec-values ([(#{x : number} #{y : number}) (values 3 4)]
		[(#{z : number}) (- x y)]
		[(#{fact : (number -> number)})
		 (lambda: ([x : number])
			  (if (zero? x) 1 (* x (fact (- x 1)))))])
	       (+ x y))

(define-values (#{x : number} #{y : number}) (values 1 2))
#;(define-values (#{z : number}) (values 1 2))

