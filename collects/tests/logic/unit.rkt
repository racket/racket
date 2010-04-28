#lang racket
(require logic
         tests/eli-tester)

(test
 (logic-var? (_))
 
 (%which () (%/= 1 1)) => #f
 (%which () (%/= 1 2)) => empty
 (%more) => #f
 
 (%which (x) (%/== x x)) => #f
 (%which (x y) (%/== x y)) => `((x . _) (y . _))
 (%more) => #f
 
 (%which () (%/== 1 1)) => #f
 (%which () (%/== 1 2)) => empty
 (%more) => #f
 
 (%which () (%< 1 2)) => empty
 (%more) => #f
 (%which () (%< 1 1)) => #f
 (%which () (%< 2 1)) => #f
 (%which () (%< 'a 2)) => #f
 (%which () (%< 1 'b)) => #f
 (%which () (%< 'a 'b)) => #f
 
 (%which () (%<= 1 2)) => empty
 (%more) => #f
 (%which () (%<= 1 1)) => empty
 (%more) => #f
 (%which () (%<= 2 1)) => #f
 (%which () (%<= 'a 2)) => #f
 (%which () (%<= 1 'b)) => #f
 (%which () (%<= 'a 'b)) => #f
 
 (%which () (%= 1 1)) => empty
 (%more) => #f
 (%which () (%= 'a 'a)) => empty
 (%more) => #f
 (%which () (%= (cons 1 2) (cons 1 2))) => empty
 (%more) => #f
 (%which () (%= (vector 1 2) (vector 1 2))) => empty
 (%more) => #f
 (%which (x) (%= x 1)) => `((x . 1))
 (%more) => #f
 (%which (x) (%= (cons x 2) (cons 1 2))) => `((x . 1))
 (%more) => #f
 (%which (x) (%= (vector x 2) (vector 1 2))) => `((x . 1))
 (%more) => #f
 (%which (x) (%and (%= x 1) (%= x 2))) => #f
 (%which () (%= 1 2)) => #f
 
 (%which () (%=/= 1 1)) => #f
 (%which () (%=/= 'a 'a)) => #f
 (%which () (%=/= 1 2)) => empty
 (%more) => #f
 
 (%which () (%=:= 1 1)) => empty
 (%more) => #f
 (%which () (%=:= 'a 'a)) => #f
 (%which () (%=:= 1 2)) => #f
 
 (%which () (%== 1 1)) => empty
 (%more) => #f
 (%which (x) (%== x x)) => `((x . _))
 (%more) => #f
 (%which (x) (%== (cons 1 x) (cons 1 x))) => `((x . _))
 (%more) => #f
 ; XXX This answer seems wrong
 (%which (x) (%and (%= x 1) (%== x 1))) => `((x . 1))
 (%more) => #f
 ; XXX This answer seems wrong
 (%which (x y) (%and (%= x 1) (%= y 1) (%== x y))) => `((x . 1) (y . 1))
 (%more) => #f
 (%which (x y) (%== x y)) => #f
 (%which (x y) (%== (cons 1 x) (cons y 2))) => #f
 
 (%which () (%> 2 1)) => empty
 (%more) => #f
 (%which () (%> 1 1)) => #f
 (%which () (%> 1 2)) => #f
 (%which () (%> 'a 2)) => #f
 (%which () (%> 1 'b)) => #f
 (%which () (%> 'a 'b)) => #f
 
 (%which () (%>= 2 1)) => empty
 (%more) => #f
 (%which () (%>= 1 1)) => empty
 (%more) => #f
 (%which () (%>= 1 2)) => #f
 (%which () (%>= 'a 2)) => #f
 (%which () (%>= 1 'b)) => #f
 (%which () (%>= 'a 'b)) => #f 
 
 (%which () (%and %true %true)) => empty
 (%more) => #f
 (%which () (%and %fail %true)) => #f
 (%more) => #f
 
 (%which () (%append empty empty empty)) => empty
 (%more) => #f
 (%which () (%append (list 1) empty (list 1))) => empty
 (%more) => #f
 (%which () (%append empty (list 2) (list 2))) => empty
 (%more) => #f
 (%which () (%append (list 1) (list 2) (list 1 2))) => empty
 (%more) => #f
 
 (let ([rel %empty-rel])
   (test (%which (y) (rel 'x y)) => #f
         (%assert! rel () [('x 1)])
         (%which (y) (rel 'x y)) => `([y . 1])
         (%more) => #f
         (%assert-after! rel () [('x 2)])
         (%which (y) (rel 'x y)) => `([y . 2])
         (%more) => `([y . 1])
         (%more) => #f
         (%assert! rel () [('x 3)])
         (%which (y) (rel 'x y)) => `([y . 2])
         (%more) => `([y . 1])
         (%more) => `([y . 3])
         (%more) => #f))
 
 (%which (y) (%let (x) (%bag-of x (%or (%= x 1) (%= x 1) (%= x 2)) y))) => `([y . (1 1 2)])
 (%more) => #f
 ; XXX I don't know a program that would get these outputs
 ;(%which (y) (%let (x) (%bag-of x XXX y))) => `([y . ()])
 (%more) => #f
 (%which (y) (%let (x) (%bag-of-1 x (%or (%= x 1) (%= x 1) (%= x 2)) y))) => `([y . (1 1 2)])
 (%more) => #f
 ; XXX I don't know a program that would get these outputs
 ;(%which (y) (%let (x) (%bag-of-1 x XXX y))) => #f
 
 (%which () (%compound (cons 1 1))) => empty
 (%more) => #f
 (%which () (%compound (vector 1 1))) => empty
 (%more) => #f
 (%which () (%let (x) (%and (%= x (cons 1 1)) (%compound x)))) => empty
 (%more) => #f
 (%which () (%compound 1)) => #f 
 (%which () (%compound "1")) => #f 
 (%which () (%compound '1)) => #f 
 (%which () (%compound empty)) => #f 
 
 (%which () (%constant (cons 1 1))) => #f
 (%which () (%constant (vector 1 1))) => #f
 (%which () (%let (x) (%and (%= x 1) (%constant x)))) => empty
 (%more) => #f
 (%which () (%constant 1)) => empty
 (%more) => #f
 (%which () (%constant "1")) => empty
 (%more) => #f
 (%which () (%constant '1)) => empty
 (%more) => #f
 (%which () (%constant empty)) => empty
 (%more) => #f
 
 (%which (x) (%let (y) (%and (%copy x y) (%= y 1)))) => `([x . _])
 (%more) => #f
 
 ! =error> "syntactically"
 
 (%which () (%cut-delimiter %true)) => empty
 (%more) => #f
 (%which () (%cut-delimiter !)) => empty
 (%more) => #f
 (%which () (%cut-delimiter %fail)) => #f
 (%which () (%or %true %true)) => empty
 (%more) => empty
 (%more) => #f
 (%which () (%cut-delimiter (%or (%and ! %true) %true))) => empty
 (%more) => #f
 
 (%which () (%empty-rel 1 1)) => #f
 
 (%which () %fail) => #f
 
 ; %free-vars example from documentation
 (local [(define %knows
           (%rel ()
                 [('Odysseus 'TeX)]
                 [('Odysseus 'Scheme)]
                 [('Odysseus 'Prolog)]
                 [('Odysseus 'Penelope)]
                 [('Penelope 'TeX)]
                 [('Penelope 'Prolog)]
                 [('Penelope 'Odysseus)]
                 [('Telemachus 'TeX)]
                 [('Telemachus 'calculus)]))]
   (test (%which (someone things-known)
                 (%let (x)
                       (%set-of x (%knows someone x)
                                things-known)))
         =>
         `((someone . _) (things-known TeX Scheme Prolog Penelope Odysseus calculus))
         (%more) => #f
         (%which (someone things-known)
                 (%let (x)
                       (%bag-of x
                                (%free-vars (someone)
                                            (%knows someone x))
                                things-known)))
         =>
         `((someone . Odysseus) (things-known TeX Scheme Prolog Penelope))
         (%more) =>
         `((someone . Penelope) (things-known TeX Prolog Odysseus))
         (%more) =>
         `((someone . Telemachus) (things-known TeX calculus))
         (%more) =>
         #f))
 
 (%which (x) (%let (y) (%and (%freeze x y) (%nonvar y)))) => `([x . _])
 
 (%which () (%if-then-else %true %true %true)) => empty
 (%more) => #f
 (%which () (%if-then-else %true %fail %true)) => #f
 (%which () (%if-then-else %fail %true %true)) => empty
 (%more) => #f
 
 (%which (x) (%is x (* 6 7))) => `([x . 42])
 (%more) => #f
 (%which (x) (%let (y) (%and (%= y 7) (%is x (* 6 y))))) => `([x . 42])
 (%more) => #f
 
 (%which () (%let (x) (%= x x))) => empty
 (%more) => #f
 
 (%which (x) (%let (y z) (%and (%freeze x y) (%melt y z) (%= z 1)))) => `([x . 1])
 (%more) => #f
 
 (%which (x) (%let (y z) (%and (%freeze x y) (%melt-new y z) (%= z 1)))) => `([x . _])
 (%more) => #f
 
 (%which () (%member 3 (list 1 2 3))) => empty
 (%more) => #f
 (%which () (%member 3 (list 1 2 3 3))) => empty
 (%more) => empty
 (%more) => #f
 (%which () (%member 3 (list 1 2))) => #f
 
 (%which () (%let (x) (%nonvar x))) => #f
 (%which () (%let (x) (%nonvar (cons 1 x)))) => #f
 (%which () (%let (x) (%nonvar (vector x)))) => #f
 (%which () (%let (x) (%nonvar 1))) => empty
 (%more) => #f
 (%which () (%let (x) (%and (%= x 1) (%nonvar x)))) => empty
 (%more) => #f
 
 (%which () (%not %true)) => #f
 (%which () (%not %fail)) => empty
 (%more) => #f
 
 (%which () (%or %true %true)) => empty
 (%more) => empty
 (%more) => #f
 (%which () (%or %true %fail %true)) => empty
 (%more) => empty
 (%more) => #f
 
 (let ([rel (%rel () [(1)] [(2) %fail])])
   (test (%which () (rel 1)) => empty
         (%more) => #f
         (%which () (rel 2)) => #f))
 (let ([rel (%rel () [(1) !] [(1) (%repeat)])])
   (test (%which () (rel 1)) => empty
         (%more) => #f))
 
 (local [(define (many-%more n)
           (if (zero? n)
               empty
               (and (%more)
                    (many-%more (sub1 n)))))]
   (test (%which () (%repeat)) => empty
         (many-%more (random 50)) => empty))
 
 (parameterize ([use-occurs-check? #f])
   (%which () (%let (x) (%= x (cons 1 x))))) 
 => empty
 (parameterize ([use-occurs-check? #t])
   (%which () (%let (x) (%= x (cons 1 x)))))
 => #f
 
 (%which (y) (%let (x) (%set-of x (%or (%= x 1) (%= x 1) (%= x 2)) y))) => `([y . (1 2)])
 (%more) => #f
 ; XXX I don't know a program that would get these outputs
 ;(%which (y) (%let (x) (%set-of x XXX y))) => `([y . ()])
 (%more) => #f
 (%which (y) (%let (x) (%set-of-1 x (%or (%= x 1) (%= x 1) (%= x 2)) y))) => `([y . (1 2)])
 (%more) => #f
 ; XXX I don't know a program that would get these outputs
 ;(%which (y) (%let (x) (%set-of-1 x XXX y))) => #f
 
 (%which () %true) => empty
 (%more) => #f
 
 (%which () (%let (x) (%var x))) => empty
 (%more) => #f
 (%which () (%let (x) (%var (cons 1 x)))) => empty
 (%more) => #f
 (%which () (%let (x) (%var (vector x)))) => empty
 (%more) => #f
 (%which () (%let (x) (%var 1))) => #f
 (%which () (%let (x) (%and (%= x 1) (%var x)))) => #f
 
 )