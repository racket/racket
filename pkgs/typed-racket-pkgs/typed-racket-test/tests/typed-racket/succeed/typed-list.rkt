#lang typed-scheme
(provide -foldl)

(pdefine: (a b) (-foldl [f : (a b -> b)] [e : b] [l : (Listof a)]) : b
	  (if (null? l)
              e
	      (foldl f (f (car l) e) (cdr l))))

