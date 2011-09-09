#lang typed-scheme
#;(require mzlib/etc)
#;(require "prims.rkt")
(require mzlib/match)

(define-type-alias number Number)
(define-type-alias boolean Boolean)
(define-type-alias symbol Symbol)
(define-type-alias top Any)
(define-type-alias list-of Listof)
(define-type-alias atom (Un Number Symbol Boolean))

(define: atom? : (Any -> Boolean : atom) (lambda: ([v : Any]) (if (number? v) #t (if (symbol? v) #t (boolean? v)))))


(define-type-alias lat (list-of atom))

(define: (member? [a : atom] [l : lat]) : boolean
  (cond
   [(null? l) #f]
   [(equal? a (car l)) #t]
   [else (member? a (cdr l))]))

(define: (two-in-a-row [l : lat]) : boolean
  (cond
   [(null? l) #f]
   [(null? (cdr l)) #f]
   [(equal? (car l) (car (cdr l))) #t]
   [else (two-in-a-row (cdr l))]))


(define: (two-in-a-row2 [l : lat]) : boolean
  (define: (two-in-a-row-b [prec : atom] [alat : lat]) : boolean
    (cond
     [(null? alat) #f]
     [else (or (eq? (car alat) prec)
	       (two-in-a-row-b (car alat) (cdr alat)))]))
  (cond
   [(null? l) #f]
   [else (two-in-a-row-b (car l) (cdr l))]))

(define-type-alias lon (list-of number))

(define: (sum-of-prefixes-b [sonssf : number] [tup : lon]) : lon
  (cond
   [(null? tup) '()]
   [else (cons (+ sonssf (car tup))
	       (sum-of-prefixes-b (+ sonssf (car tup))
				  (cdr tup)))]))

(define: (sum-of-prefixes [tup : lon]) : lon
  (sum-of-prefixes-b 0 tup))

(define: (one? [n : number]) : boolean
  (= n 1))

(pdefine: (e) (pick [n : number] [lat : (list-of e)]) : e
	  (cond [(one? n) (car lat)]
		[else (pick (sub1 n) (cdr lat))]))

(define: (scramble-b [tup : lon] [rev-pre : lon]) : lon
  (cond [(null? tup) '()]
	[else (cons (pick (car tup) (cons (car tup) rev-pre))
		    (scramble-b (cdr tup)
				(cons (car tup) rev-pre)))]))

(define: (scramble [tup : lon]) : lon
  (scramble-b tup '()))

(pick 2 (cons 'a (cons 'd (cons 'c #{'() : (list-of symbol)}))))

(define: (multirember [a : atom] [l : lat]) : lat
  (letrec ([#{mr : (lat -> lat)}
	    (lambda: ([l : lat])
		     (cond [(null? l) l]
			   [(eq? a (car l)) (mr (cdr l))]
			   [else (cons (car l) (mr (cdr l)))]))])
    (mr l)))

(pdefine: (e) (multirember-f [f : (e e -> boolean)] [a : e] [l : (list-of e)]) : (list-of e)
	  (let: mr : (list-of e) ([l : (list-of e) l])
		(cond [(null? l) l]
		      [(f a (car l)) (mr (cdr l))]
		      [else (cons (car l) (mr (cdr l)))]))
	  #;(letrec ([#{mr : ((list-of e) -> (list-of e))}
	  (lambda: ([l : (list-of e)])
	  (cond [(null? l) l]
	  [(f a (car l)) (mr (cdr l))]
	  [else (cons (car l) (mr (cdr l)))]))])
	  (mr l)))

(define-type-alias set lat)

(define: (union [set1 : set] [set2 : set]) : set
  (cond [(null? set1) set2]
	[(member? (car set1) set2) (union (cdr set1) set2)]
	[else (cons (car set1) (union (cdr set1) set2))]))

(define: (intersect [set1 : set] [set2 : set]) : set
  (define: (I [set1 : set]) : set
    (cond [(null? set1) set1]
	  [(member? (car set1) set2) (cons (car set1) (I (cdr set1)))]
	  [else (I (cdr set1))]))
  (cond [(null? set2) set2]
	[else (I set1)]))

