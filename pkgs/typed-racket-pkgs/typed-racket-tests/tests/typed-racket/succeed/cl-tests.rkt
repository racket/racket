#lang typed-scheme
(define-type-alias number Number)
(define-type-alias boolean Boolean)
(define-type-alias top Any)

(define: a : (number -> number) (lambda: ([x : number]) x))
(define: f : (case-lambda (number -> number)
			  (boolean boolean -> boolean))
  (case-lambda
   [(#{x : number}) (add1 x)]
   [(#{a : boolean} #{b : boolean}) (and a b)]))

(define: f* : (case-lambda (number -> number)
			   (boolean boolean -> boolean))
  (case-lambda:
   [([x : number]) (add1 x)]
   [([a : boolean] [b : boolean]) (and a b)]))

(f 5)

(f #t #f)

#;(f #t)

(define-type-alias idfunty (All (a) (a -> a)))
(define-type-alias (idfunty2 a) (a -> a))

(define: g : (idfunty number) (lambda: ([x : number]) x))

(define: (h [f : (idfunty number)]) : number (f 5))
(define: (h* [f : (idfunty2 number)]) : number (f 5))

(h f*)
(h* f*)

