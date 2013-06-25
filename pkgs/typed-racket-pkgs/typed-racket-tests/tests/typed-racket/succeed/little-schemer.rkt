#lang typed-scheme
#;(require mzlib/etc)
#;(require "prims.rkt")
(require mzlib/match
         (for-syntax scheme/base))

(define-type-alias number Number)
(define-type-alias boolean Boolean)
(define-type-alias symbol Symbol)
(define-type-alias top Any)
(define-type-alias list-of Listof)

(define-type-alias atom (Un Number Symbol Boolean))

(define: atom? : (Any -> Boolean : atom) (lambda: ([v : Any]) (if (number? v) #t (if (symbol? v) #t (boolean? v)))))

(define-syntax (cond* stx)
  (syntax-case stx (else)
    [(_ [pred expr id rhs] . rest)
     (quasisyntax/loc stx
         (let ([id expr])
           (if (pred id)
               rhs
               #,(syntax/loc #'rest (cond . rest)))))]
    [(_ [else . rest]) #'(begin . rest)]
    [(_ [p . rhs] . rest)
     #'(if p (begin . rhs)
           (cond* . rest))]))

(define-type-alias lat (list-of atom))

(define: (lat? [l : (list-of top)]) : boolean
  (cond [(null? l) #t]
	[(atom? (car l)) (lat? (cdr l))]
	[else #f]))

(define: (member? [a : atom] [l : lat]) : boolean
  (cond
   [(null? l) #f]
   [else (or (equal? a (car l))
	     (member? a (cdr l)))]))

(define: (rember [a : symbol] [l : (list-of symbol)]) : (list-of symbol)
  (cond
   [(null? l) l]
   [(eq? (car l) a) (cdr l)]
   [else (cons (car l) (rember a (cdr l)))]))

(define: (multisubst [new : symbol] [old : symbol] [lat : (list-of symbol)]) : (list-of symbol)
  (cond
   [(null? lat) lat]
   [(eq? (car lat) old) (cons new (multisubst new old (cdr lat)))]
   [else (cons (car lat) (multisubst new old (cdr lat)))]))

(define: (tup+ [t1 : (list-of number)] [t2 : (list-of number)]) : (list-of number)
  (cond
   [(null? t1) t2]
   [(null? t2) t1]
   [else (cons (+ (car t1) (car t2))
	       (tup+ (cdr t1) (cdr t2)))]))

(define: (len [l : (list-of top)]) : number
  (cond
   [(null? l) 0]
   [else (add1 (len (cdr l)))]))

(define: (pick [n : number] [lat : (list-of symbol)]) : symbol
  (cond
   [(zero? (sub1 n)) (car lat)]
   [else (pick (sub1 n) (cdr lat))]))

(define: (no-nums [lat : (list-of atom)]) : (list-of atom)
  (cond
   [(null? lat) lat]
   [(number? (car lat)) (no-nums (cdr lat))]
   [else (cons (car lat) (no-nums (cdr lat)))]))

(define: (one? [n : number]) : boolean
  (= n 1))

(define: (rempick [n : number] [lat : (list-of atom)]) : (list-of atom)
  (cond
   [(one? n) (cdr lat)]
   [else (cons (car lat)
	       (rempick (sub1 n) (cdr lat)))]))

(define: (foo2 [x : top]) : boolean
  (if (number? x) (= x x) #f))

;; doesn't work because of and! - bug in type system
(define: (eqan? [a1 : top] [a2 : top]) : boolean
  (cond [(and (number? a1) (number? a2)) (= a1 a2)]
	[else (eq? a1 a2)]))

(define: (occur [a : atom] [lat : (list-of atom)]) : number
  (cond [(null? lat) 0]
	[(eq? (car lat) a) (add1 (occur a (cdr lat)))]
	[else (occur a (cdr lat))]))

(define-type-alias SExp (mu x (U atom (Listof x))))


;; (atom? (car l)) doesn't do anything - bug in type system
#;(define: (rember* [a : atom] [l : (list-of SExp)]) : (list-of SExp)
(cond
 [(null? l) l]
 [(atom? (car l))
  (cond [(eq? (car l) a) (rember* a (cdr l))]
	[else (cons (car l) (rember* a (cdr l)))])]
 [else (cons (rember* a (car l)) (rember* a (cdr l)))]))

(define: (rember* [a : atom] [l : (list-of SExp)]) : (list-of SExp)
  (cond
   [(null? l) l]
   [else
    (let ([c (car l)])
      (cond
        [(atom? c)
         (cond [(eq? c a) (rember* a (cdr l))]
               [else (cons c (rember* a (cdr l)))])]
        [else (cons (rember* a c) (rember* a (cdr l)))]))]))

(define: (insertR* [new : atom] [old : atom] [l : (list-of SExp)]) : (list-of SExp)
  (cond
   [(null? l) l]
   [else
    (let ([c (car l)])
      (cond
        [(atom? c)
         (cond
           [(eq? c old)
            (cons old (cons new (insertR* new old (cdr l))))]
           [else
            (cons c
                  (insertR* new old (cdr l)))])]
        [else (cons (insertR* new old c)
                    (insertR* new old (cdr l)))]))]))

(define: (occur* [a : atom] [l : (list-of SExp)]) : number
  (cond*
   [(null? l) 0]
   [atom? (car l) c
	  (cond [(eq? c a) (add1 (occur* a (cdr l)))]
		[else (occur* a (cdr l))])]
   [else (+ (occur* a c)
	    (occur* a (cdr l)))]))

(define: (member* [a : atom] [l : (list-of SExp)]) : boolean
  (cond*
   [(null? l) #f]
   [atom? (car l) c
	  (or (eq? a c) (member* a (cdr l)))]
   [else (or (member* a c) (member* a (cdr l)))]))

(define: (^ [n : number] [m : number]) : number
  (if (= m 0) 1 (* n (^ n (sub1 m)))))

(define: (1st-sub-exp [ae : (list-of SExp)]) : SExp
  (car ae))

(define: (2nd-sub-exp [ae : (list-of SExp)]) : SExp
  (car (cdr (cdr ae))))

(define: (operator [ae : (list-of SExp)]) : SExp
  (car (cdr ae)))

(define-type-alias num-exp (Rec N (U Number (List N (U '+ '* '^) N))))

(define: (value [nexp : num-exp]) : number
  (cond
   [(atom? nexp) nexp]
   [(eq? (car (cdr nexp)) '+)
    (+ (value (car nexp))
       (value (car (cdr (cdr nexp)))))]
   [(eq? (car (cdr nexp)) '*)
    (* (value (car nexp))
       (value (car (cdr (cdr nexp)))))]
   [else
    (^ (value (car nexp))
       (value (car (cdr (cdr nexp)))))]
   ))

#;(define-type aexp (Un atom (list-of aexp)))

(define: (set? [l : (list-of atom)]) : boolean
  (cond
   [(null? l) #t]
   [(member? (car l) (cdr l)) #f]
   [else (set? (cdr l))]))

(define: (multirember [a : atom] [l : (list-of atom)]) : (list-of atom)
  (cond
   [(null? l) l]
   [(equal? a (car l)) (multirember a (cdr l))]
   [else (cons (car l) (multirember a (cdr l)))]))

(define: (makeset [l : lat]) : lat
  (cond
   [(null? l) l]
   [else (cons (car l)
	       (makeset (multirember (car l) (cdr l))))]))

(define: (subset? [set1 : lat] [set2 : lat]) : boolean
  (cond
   [(null? set1) #t]
   [(member? (car set1) set2)
    (subset? (cdr set1) set2)]
   [else #f]))

(define: (subset2? [set1 : (list-of atom)] [set2 : (list-of atom)]) : boolean
  (cond
   [(null? set1) #t]
   [else (and (member? (car set1) set2)
	      (subset? (cdr set1) set2))]))

(define: (intersect? [set1 : (list-of atom)] [set2 : (list-of atom)]) : boolean
  (cond
   [(null? set1) #t]
   [else (or (member? (car set1) set2)
	     (intersect? (cdr set1) set2))]))

(define: (eqset? [set1 : (list-of atom)] [set2 : (list-of atom)]) : boolean
  (and (subset? set1 set2) (subset? set2 set1)))

(define: (intersect [set1 : (list-of atom)] [set2 : (list-of atom)]) : (list-of atom)
  (cond
   [(null? set1) set1]
   [(member? (car set1) set2)
    (cons (car set1) (intersect (cdr set1) set2))]
   [else (intersect (cdr set1) set2)])
  )

(define: (union [set1 : (list-of atom)] [set2 : (list-of atom)]) : (list-of atom)
  (cond
   [(null? set1) set2]
   [(member? (car set1) set2)
    (union (cdr set1) set2)]
   [else (cons (car set1) (intersect (cdr set1) set2))])
  )

(define: (rember-f [test? : (atom atom -> boolean)] [a : atom] [l : (list-of atom)]) : (list-of atom)
  (cond
   [(null? l) l]
   [(test? (car l) a) (cdr l)]
   [else (cons (car l) (rember-f test? a (cdr l)))]))

(define: (rember-f-curry [test? : (atom atom -> boolean)]) : (atom lat -> lat)
  (lambda: ([a : atom] [l : (list-of atom)])
	   (cond
	    [(null? l) l]
	    [(test? (car l) a) (cdr l)]
	    [else (cons (car l) ((rember-f-curry test?) a (cdr l)))])))

(define: eq?-c : (atom -> (atom -> boolean))
  (lambda: ([a : atom])
	   (lambda: ([x : atom])
		    (eq? x a))))

(define: (insertR-f [test? : (atom atom -> boolean)] [new : atom] [old : atom] [l : (list-of atom)]) : (list-of atom)
  (cond
   [(null? l) l]
   [(test? (car l) old)
    (cons old (cons new (cdr l)))]
   [else (cons (car l)
	       (insertR-f test? new old (cdr l)))]))

(define: (seqL [new : atom] [old : atom] [l : (list-of atom)]) : (list-of atom)
  (cons new (cons old l)))


(define: (seqR [new : atom] [old : atom] [l : (list-of atom)]) : (list-of atom)
  (cons old (cons new l)))

(define: (insertR-g [seq : (atom atom lat -> lat)]
		    [test? : (atom atom -> boolean)]
		    [new : atom] [old : atom] [l : (list-of atom)])
  : (list-of atom)
  (cond
   [(null? l) l]
   [(test? (car l) old)
    (seq new old (cdr l))]
   [else (cons (car l)
	       (insertR-g seq test? new old (cdr l)))]))

(define: (insertR-g-curry [seq : (atom atom (list-of atom) -> (list-of atom))])
  : ((atom atom -> boolean) atom atom (list-of atom) -> (list-of atom))
  (lambda: ([test? : (atom atom -> boolean)]
	    [new : atom] [old : atom] [l : (list-of atom)])
	   (cond
	    [(null? l) l]
	    [(test? (car l) old)
	     (seq new old (cdr l))]
	    [else (cons (car l)
			((insertR-g-curry seq) test? new old (cdr l)))])))

(define: (seqS [new : atom] [old : atom] [l : lat]) : lat
  (cons new l))

#;(define: subst : ((atom atom -> boolean) atom atom (list-of atom) -> (list-of atom))
(insertR-g-curry seqS))

(define: (atom->function [x : atom]) : (number number -> number)
  (case x
    [(+) +]
    [(*) *]
    [else ^]))

;; doesn't work - is operator really a number? (bug in type system)
;; also infinite loops checking num-exp <: SExp
#;(define: (value-new [nexp : num-exp]) : number
(cond
 [(number? nexp) nexp]
 [else ((atom->function (operator nexp))
	(value-new (1st-sub-exp nexp))
	(value-new (2nd-sub-exp nexp)))]))

(define: (multiremberT [test? : (atom -> boolean)] [l : (list-of atom)]) : (list-of atom)
  (cond
   [(null? l) l]
   [(test? (car l)) (multiremberT test? (cdr l))]
   [else (cons (car l) (multiremberT test? (cdr l)))]))

(define: (build [a : SExp] [b : SExp]) : (list-of SExp)
  (cons a (cons b '())))

(define: (first [pair : (list-of SExp)]) : SExp
  (car pair))

(define: (second [pair : (list-of SExp)]) : SExp
  (car (cdr pair)))

;; need to specify more about the list in the type here  - type system bug
#;(define: (shift [pair : (list-of SExp)]) : (list-of SExp)
(build (first (first pair))
       (build (second (first pair))
	      (second pair))))

;; changed to test for exact-integer? before even? check.
(define: (collatz [n : number]) : number
  (cond [(one? n) 1]
	[(and (exact-integer? n) (even? n)) (collatz (/ n 2))]
	[else (collatz (add1 (* 3 n)))]))


(define: (ack [n : number] [m : number]) : number
  (cond
   [(zero? n) (add1 m)]
   [(zero? m) (ack (sub1 n) 1)]
   [else (ack (sub1 n)
	      (ack n (sub1 m)))]))


					;(define-type-alias entry (list-of (list-of atom)))

(define: empty-atom : (list-of (list-of atom)) '())

;; FIXME
(define: mymap : (All (a b) ((a -> b) (list-of a) -> (list-of b)))
  (plambda: (a b) ([f : (a -> b)] [l : (list-of a)])
    (cond [(null? l) '()]
          [else (cons (f (car l))
                      (mymap f (cdr l)))])))

(mymap add1 (cons 1 (cons 2 (cons 3 '()))))

(define-type-alias entry (list-of lat))

(define-type-alias table (list-of entry))


(define: (new-entry  [keys : (list-of atom)]
		     [vals : (list-of atom)]) : entry
		     (cons keys (cons vals empty-atom)))

(define: (numbered? [aexp : num-exp]) : boolean
  (cond
   [(number? aexp) #t]
   [(atom? aexp) #f]
   [else (and (numbered? (car aexp))
	      (numbered? (car (cdr (cdr aexp)))))]))

(define: (lookup-in-entry-help [name : atom] [names : (list-of atom)] [values : (list-of atom)] [entry-f : (atom -> atom)]) : atom
  (cond [(null? names) (entry-f name)]
	[(eq? (car names) name) (car values)]
	[else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)]))

(define: (lookup-in-entry [name : atom] [e : entry] [fail : (atom -> atom)]) : atom
  (lookup-in-entry-help name (car e) (car (cdr e)) fail))

(define: extend-table : (entry table -> table) #{cons @ entry Any})

(define: (lookup-in-table [name : atom] [t : table] [fail : (atom -> atom)]) : atom
  (cond
   [(null? t) (fail name)]
   [else (lookup-in-entry
	  name
	  (car t)
	  (lambda: ([name : atom])
		   (lookup-in-table name (cdr t) fail)))]))

(define-type-alias action (atom table -> SExp))

(define: (*const [e : SExp] [t : table]) : SExp
  (cond
   [(number? e) e]
   [(eq? e #t) #t]
   [(eq? e #f) #f]
   [else (build 'primitive e)]))

(define: (initial-table [name : atom]) : atom
  (error 'fail))

(define: (*identifier [e : atom] [tbl : table]) : SExp
  (lookup-in-table e tbl initial-table))

(define: (atom->action [e : atom]) : action
  (cond
   [(number? e) *const]
   #;[(string? e) (error "shouldn't get strings")] ;; FIXME - had to change the code
   [else
    (case e
      [(#t #f cons car cdr null? eq? atom? zero? add1 sub1 number?) *const]
      [else *identifier])]))

(define: (*quote [a : atom] [t : table]) : SExp (error 'fail))
(define: (*lambda [a : atom] [t : table]) : SExp (error 'fail))
(define: (*cond [a : atom] [t : table]) : SExp (error 'fail))
(define: (*application [a : atom] [t : table]) : SExp (error 'fail))

(define: (list->action [e : (list-of SExp)]) : action
  (cond*
   [atom? (car e) it
	  (case it
	    [(quote) *quote]
	    [(lambda) *lambda]
	    [(cond) *cond]
	    [else *application])]
   [else *application]))


(define: (expression->action [e : SExp]) : action
  (cond
   [(atom? e) (atom->action e)]
   [else (list->action e)]))

#;(define: (meaning [e : SExp] [t : table]) : SExp
((expression->action e) e t))

#;(define: (value [e : SExp]) : SExp
(meaning e '()))

#;(provide (all-defined))



