#lang typed-scheme
(define-type-alias number Number)
(define-type-alias boolean Boolean)
(define-type-alias top Any)

(define: f : (case-lambda [number -> number] [boolean boolean -> boolean])
  (case-lambda [(#{a : number}) a]
	       [(#{b : boolean} #{c : boolean}) (and b c)]))
