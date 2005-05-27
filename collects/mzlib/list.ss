
(module list mzscheme
  (require "spidey.ss")

  (provide set-first!
	  first
	  second
	  third
	  fourth
	  fifth
	  sixth
	  seventh
	  eighth

	  set-rest!
	  rest

	  cons?
	  empty
	  empty?

	  foldl
	  foldr

	  last-pair

	  remv
	  remq
	  remove
	  remv*
	  remq*
	  remove*

	  assf
	  memf

	  filter
	  
	  quicksort
	  mergesort)
  
  (define quicksort
    (polymorphic
     (lambda (l less-than)
       (unless (list? l)
	 (raise-type-error 'quicksort "proper list" l))
       (unless (procedure-arity-includes? less-than 2)
	 (raise-type-error 'quicksort "procedure of arity 2" less-than))
       (let* ([v (list->vector l)]
              [count (vector-length v)])
	 (let loop ([min 0][max count])
           (if (< min (sub1 max))
               (let ([pval (vector-ref v min)])
                 (let pivot-loop ([pivot min]
                                  [pos (add1 min)])
                   (if (< pos max)
                       (let ([cval (vector-ref v pos)])
                         (if (less-than cval pval)
                             (begin
                               (vector-set! v pos (vector-ref v pivot))
                               (vector-set! v pivot cval)
                               (pivot-loop (add1 pivot) (add1 pos)))
                             (pivot-loop pivot (add1 pos))))
                       (if (= min pivot)
                           (loop (add1 pivot) max)
                           (begin
                             (loop min pivot)
                             (loop pivot max))))))))
         (vector->list v)))))

  (define mergesort
    (polymorphic
     (lambda (alox less-than)
       (letrec ([split (lambda (alox r)
			 (cond
			  [(null? alox) r]
			  [(null? (cdr alox)) (cons alox r)]
			  [else (split (cdr alox) (cons (list (car alox)) r))]))]
		[merge (lambda (l1 l2 r)
			 (cond
			  [(null? l1) (append! (reverse! r) l2)]
			  [(null? l2) (append! (reverse! r) l1)]
			  [(less-than (car l1) (car l2)) 
			   (merge (cdr l1) l2 (cons (car l1) r))]
			  [else (merge (cdr l2) l1 (cons (car l2) r))]))]
		[map2 (lambda (l)
			(cond
			 [(null? l) '()]
			 [(null? (cdr l)) l]
			 [else (cons (merge (car l) (cadr l) null) 
				     (map2 (cddr l)))]))]
		[until (lambda (l)
			 (if (null? (cdr l)) 
			     (car l) 
			     (until (map2 l))))])
	 (if (null? alox)
	     null
	     (until (split alox null)))))))

  (define remove
    (polymorphic
     (letrec ([rm (case-lambda 
                   [(item list) (rm item list equal?)]
                   [(item list equal?)
                    (let loop ([list list])
                      (cond
			[(null? list) ()]
			[(equal? item (car list)) (cdr list)]
			[else (cons (car list)
				    (loop (cdr list)))]))])])
       rm)))
  
  (define remq
    (polymorphic
     (lambda (item list)
       (remove item list eq?))))
  
  (define remv
    (polymorphic
     (lambda (item list)
       (remove item list eqv?))))
  
  (define remove*
    (polymorphic
     (case-lambda
      [(l r equal?)
       (cond 
	[(null? r) null]
	[else (let ([first-r (car r)])
		(let loop ([l-rest l])
		  (cond 
		   [(null? l-rest) (cons first-r (remove* l (cdr r) equal?))]
		   [(equal? (car l-rest) first-r) (remove* l (cdr r) equal?)]
		   [else (loop (cdr l-rest))])))])]
      [(l r) (remove* l r equal?)])))
  
  (define remq*
    (polymorphic
     (lambda (l r)
       (remove* l r eq?))))
  
  (define remv*
    (polymorphic
     (lambda (l r)
       (remove* l r eqv?))))
  
  ;; fold : ((A B -> B) B (listof A) -> B)
  ;; fold : ((A1 ... An B -> B) B (listof A1) ... (listof An) -> B)
  
  ;; foldl builds "B" from the beginning of the list to the end of the
  ;; list and foldr builds the "B" from the end of the list to the
  ;; beginning of the list.
  
  (define mapadd
    (polymorphic
     (lambda (f l last)
       (letrec ((helper
                 (lambda (l)
                   (cond
		     [(null? l) (list last)]
		     [else (cons (f (car l)) (helper (cdr l)))]))))
         (helper l)))))
  
  (define foldl
    (polymorphic
     (letrec ((fold-one
               (lambda (f init l)
                 (letrec ((helper
                           (lambda (init l)
                             (cond
			       [(null? l) init]
			       [else (helper (f (car l) init) (cdr l))]))))
                   (helper init l))))
              (fold-n
               (lambda (f init  l)
                 (cond
		   [(ormap null? l)
		    (if (andmap null? l) 
			init
			(error 'foldl "received non-equal length input lists"))]
		   [else (fold-n
			  f
			  (apply f (mapadd car l init))
			  (map cdr l))]))))
       (case-lambda
        [(f init l) (fold-one f init l)]
        [(f init l . ls) (fold-n f init (cons l ls))]))))
  
  (define foldr
    (polymorphic
     (letrec ((fold-one
               (lambda (f init l)
                 (letrec ((helper
                           (lambda (init l)
                             (cond
			       [(null? l) init]
			       [else (f (car l) (helper init (cdr l)))]))))
                   (helper init l))))
              (fold-n
               (lambda (f init l)
                 (cond
		   [(ormap null? l)
		    (if (andmap null? l)
			init
			(error 'foldr "received non-equal length input lists"))]
		   [else (apply f
				(mapadd car l
					(fold-n f init (map cdr l))))]))))
       (case-lambda
        [(f init l) (fold-one f init l)]
        [(f init l . ls) (fold-n f init (cons l ls))]))))
  
  (define make-find
    (lambda (name whole-list?)
      (polymorphic
       (lambda (f list)
         (unless (and (procedure? f)
                      (procedure-arity-includes? f 1))
           (raise-type-error name "procedure (arity 1)" f))
         (let loop ([l list])
           (cond
	     [(null? l) #f]
	     [(not (pair? l)) 
	      (raise (make-exn:fail:contract
		      (format "~a: second argument must be a (proper) list; given ~e" name list)
		      (current-continuation-marks)))]
	     [else (let ([a (car l)])
		     (if whole-list?
			 (if (f a)
			     l
			     (loop (cdr l)))
			 (if (pair? a)
			     (if (f (car a))
				 a
				 (loop (cdr l)))
			     (raise-mismatch-error
			      name
			      "found a non-pair in the list: "
			      a))))]))))))

  (define assf
    (let ([a (make-find 'assf #f)])
      (polymorphic
       (lambda (f l)
	 (a f l)))))
  
  (define memf
    (let ([a (make-find 'memf #t)])
      (polymorphic
       (lambda (f l)
	 (a f l)))))
  
  (define filter
    (polymorphic
     (lambda (f list)
       (unless (and (procedure? f)
                    (procedure-arity-includes? f 1))
         (raise-type-error 'filter "procedure (arity 1)" f))
       ;; We use the reverse! trick because it's too easy to
       ;;  overflow the internal stack using natural recursion.
       ;; It's too bad that our Scheme system is so bad, but
       ;;  until someone fixes it...
       (let loop ([l list][result null])
         (cond
	   [(null? l) (reverse! result)]
	   [(pair? l)
	    (loop (cdr l) (if (f (car l))
			      (cons (car l) result)
			      result))]
	   [else (raise-mismatch-error
		  'filter
		  "expects a proper list: "
		  list)])))))
  
  (define first (polymorphic (lambda (x) 
                               (unless (pair? x)
                                 (raise-type-error 'first "non-empty list" x))
                               (car x))))
  (define set-first! 
    (polymorphic (lambda (x v)
                   (unless (pair? x)
                     (raise-type-error 'set-first! "non-empty list" x))
                   (set-car! x v))))
  (define (lget name npos)
    (lambda (x)
      (let loop ([l x][pos npos])
	(cond
	 [(and (= pos 1) (pair? l))
	  (car l)]
	 [(pair? l)
	  (loop (cdr l) (sub1 pos))]
	 [else
	  (raise-type-error name 
			    (format "list with ~a or more items" npos) 
			    x)]))))

  ;; Gives the function a name:
  (define-syntax (mk-lget stx)
    (syntax-case stx ()
      [(_ name pos)
       (syntax (polymorphic (let ([g (lget 'name pos)])
			      (lambda (x) (g x)))))]))

  (define second (mk-lget second 2))
  (define third (mk-lget third 3))
  (define fourth (mk-lget fourth 4))
  (define fifth (mk-lget fifth 5))
  (define sixth (mk-lget sixth 6))
  (define seventh (mk-lget seventh 7))
  (define eighth (mk-lget eighth 8))
  
  (define rest (polymorphic (lambda (x) 
                              (unless (pair? x)
                                (raise-type-error 'rest "non-empty list" x))
                              (cdr x))))
  
  (define set-rest! (polymorphic (lambda (x v)
                                   (unless (pair? x)
                                     (raise-type-error 'set-rest! "non-empty list" x))
                                   (unless (or (null? v) (pair? v))
                                     (raise-type-error 'set-rest! "second argument must be a list" v))
                                   (set-cdr! x v))))
    
  (define last-pair
    (polymorphic
     (lambda (l)
       (if (pair? l)
	   (let loop ((l l) (x (cdr l)))
	     (if (pair? x)
		 (loop x (cdr x))
		 l))
	   (raise-type-error 'last-pair "pair" l)))))

  (define cons? (lambda (x) (pair? x)))
  (define empty? (lambda (x) (null? x)))
  (define empty '()))

