#lang typed-scheme


(define-type-alias number Number)
(define-type-alias boolean Boolean)
(define-type-alias symbol Symbol)
(define-type-alias top Any)
(define-type-alias list-of Listof)
(+ (+)
   (+ 1 2)
   (+ 1)
   (+ 3 4 5 6)
   (- 1)
   (- 1 2)
   (- 3 4 5 6 7 (+ 45)))

(apply + '(2 3 4))


(define: f : (number boolean * -> number)
  (lambda: ([x : number] . [y : boolean *])
           (if (and (pair? y) (car y)) x (- x))))

(define: f-cl : (number boolean * -> number)
  (case-lambda: [([x : number] . [y : boolean *])
		 (if (and (pair? y) (car y)) x (- x))]))

(define: (f* [x : number] . [y : boolean *]) : number
  (if (and (pair? y) (car y)) x (- x)))

(f 3)

(f 3 #t #f)

(apply f 3 '(#f))

(f* 3)

(f* 3 #t #f)

(apply f* 3 '(#f))

(f-cl 3)

(f-cl 3 #t #f)

(apply f-cl 3 '(#f))

