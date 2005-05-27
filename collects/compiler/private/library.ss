;; Library of functions for the compiler
;; (c) 1996-7 Sebastian Good
;; (c) 1997-8 PLT, Rice University

(module library mzscheme
  (require (lib "unitsig.ss")
	  (lib "list.ss")
	  (lib "etc.ss"))

  (require (lib "zodiac-sig.ss" "syntax"))

  (require "sig.ss")

  (provide library@)
  (define library@
    (unit/sig compiler:library^
      (import (zodiac : zodiac^))

      (define logical-inverse
	(lambda (fun)
	  (lambda (obj)
	    (not (fun obj)))))

      (define one-of
	(case-lambda
	 [(p1 p2) (lambda (obj)
		    (or (p1 obj) (p2 obj)))]
	 [preds 
	  (lambda (obj)
	    (ormap (lambda (p) (p obj)) preds))]))

      (define all-of
	(lambda preds
	  (lambda (obj)
	    (andmap (lambda (p) (p obj)) preds))))

      (define none-of
	(lambda preds
	  (logical-inverse (apply one-of preds))))

      (define vector-map ; modified by PAS, but looks to be unused
	(lambda (f vec)
	  (let* ([vec-len (vector-length vec)]
		 [x (make-vector vec-len)])
	    (let loop ((i 0))
	      (if (>= i vec-len)
		  x
		  (begin (vector-set! x i (f (vector-ref vec i)))
			 (loop (add1 i))))))))

      (define improper-map
	(lambda (f ilist)
	  (cond 
	   ((pair? ilist) (cons (f (car ilist)) (improper-map f (cdr ilist))))
	   ((null? ilist) null)
	   (else (f ilist)))))

      (define begin-map!
	(lambda (non-tail tail list)
	  (if (null? list)
	      null
	      (begin
		(let loop ([list list] [next (cdr list)])
		  (let ([tail? (null? next)])
		    (set-car! list ((if tail? tail non-tail) (car list)))
		    (unless tail? (loop next (cdr next)))))
		list))))

      (define begin-map
	(lambda (non-tail tail list)
	  (if (null? list)
	      null
	      (let ([tail? (null? (cdr list))])
		(cons ((if tail? tail non-tail) (car list)) 
		      (begin-map non-tail tail (cdr list)))))))

      (define map!
	(lambda (fun list)
	  (let loop ([l list])
	    (if (null? l)
		list
		(begin (set-car! l (fun (car l))) (loop (cdr l)))))))

      (define list-index
	(lambda (obj list)
	  (cond
	   [(null? list) (error 'list-index "~a not found int ~a" obj list)]
	   [(eq? obj (car list)) 0]
	   [else (add1 (list-index obj (cdr list)))])))

      (define list-last
	(lambda (list)
	  (if (null? list)
	      (error 'list-last "~a is empty!" list)
	      (let loop ([a list] [b (cdr list)])
		(if (null? b)
		    (car a)
		    (loop b (cdr b)))))))

      ;; Set operations

      ;; -----> Begin bit-vector implementation <-----
      #|
      (define set-next-index 0)
      (define index-vector (make-vector 100))
      (define singleton-vector (make-vector 100))
      (define index-table (make-hash-table))

      (define (index->object i) (vector-ref index-vector i))
      (define (object->index o) 
	(let ([i (hash-table-get index-table o (lambda () #f))])
	  (or i
	      (let ([i set-next-index])
		(set! set-next-index (add1 set-next-index))
		(unless (< i (vector-length index-vector))
		  (printf "grow ~a~n" i)
		  (let* ([old-iv index-vector]
			 [old-sv singleton-vector]
			 [old-size (vector-length index-vector)]
			 [new-size (* 2 old-size)])
		    (set! index-vector (make-vector new-size))
		    (set! singleton-vector (make-vector new-size))
		    (let loop ([n 0])
		      (unless (= n old-size)
			(vector-set! index-vector n (vector-ref old-iv n))
			(vector-set! singleton-vector n (vector-ref old-sv n))
			(loop (add1 n))))))
		(vector-set! index-vector i o)
		(vector-set! singleton-vector i (arithmetic-shift 1 i))
		(hash-table-put! index-table o i)
		i))))
      (define (object->singleton o) 
	(let ([i (object->index o)])
	  (vector-ref singleton-vector i)))
      (define (set->objects s)
	(letrec ([dloop ; double-search
		  (lambda (s i n d)
		    (if (zero? s)
			null
			(if (positive? (bitwise-and s i))
			    (if (= n 1)
				(cons (index->object d)
				      (dloop (arithmetic-shift s -1) 1 1 (add1 d)))		
				(let ([n/2 (quotient n 2)])
					; It's in d+n/2...d+n
				  (bloop (arithmetic-shift s (- n/2)) (arithmetic-shift i (- n/2)) n/2 (+ d n/2))))
			    (dloop s (bitwise-ior i (arithmetic-shift i n)) (* n 2) d))))]
		 [bloop 
		  (lambda (s i n d)
		    (if (= n 1)
			(cons (index->object d)
			      (dloop (arithmetic-shift s -1) 1 1 (add1 d)))		
			(let* ([n/2 (quotient n 2)]
			       [low_i (arithmetic-shift i (- n/2))])
			  (if (positive? (bitwise-and s low_i))
			      (bloop s low_i n/2 d)
			      (bloop (arithmetic-shift s (- n/2)) low_i n/2 (+ d n/2))))))])
	  (dloop s 1 1 0)))

      (define (set->list s) (reverse! (set->objects s))) ; something relies on the order
      (define empty-set 0)
      (define make-singleton-set object->singleton)
      (define (list->set l)
	(let loop ([l l][s 0])
	  (if (null? l)
	      s
	      (loop (cdr l) (set-union s (object->singleton (car l)))))))
      (define (set-memq? o s)
	(positive? (bitwise-and s (object->singleton o))))

      (define set-union bitwise-ior)
      (define set-intersect bitwise-and)
      (define (set-union-singleton s o) (set-union s (object->singleton o)))
      (define (set-minus s1 s2) (bitwise-and s1 (bitwise-not s2)))
      (define (set-subset? s1 s2) (zero? (bitwise-xor s1 (bitwise-and s1 s2))))
      (define set-empty? zero?)
      (define set? integer?) ; cheat
      |#
      ;; -----> End bit-vector implementation <------

      ;; -----> Begin list implementation <------

      (define-struct set (%m))
      (define empty-set (make-set null))
      (define make-singleton-set (compose make-set list))
      (define list->set
	(lambda (l)
					; (unless (list? l) (error 'list->set "~a not a list" l))
	  (make-set l)))
      (define set->list set-%m)
      (define set-memq?
	(lambda (obj set)
	  (memq obj (set->list set))))
      (define set-empty? (compose null? set->list))

      (define set-union ; O(|a|*|b|)
	(lambda (a b)
	  (let union ([a (set->list a)]
		      [b (set->list b)])
	    (cond
	     [(null? a) (list->set b)]
	     [(memq (car a) b) (union (cdr a) b)]
	     [else (union (cdr a) (cons (car a) b))]))))

      (define set-union-singleton
	(lambda (set obj)
	  (when (void? obj)
	    (error 'stop))
	  (if (memq obj (set->list set))
	      set
	      (list->set (cons obj (set->list set))))))

      (define set-minus ; O(|a|*|b|)
	(lambda (a b)
	  (let minus ([a (set->list a)]
		      [b (set->list b)]
		      [acc null])
	    (cond
	     [(null? a) (list->set acc)]
	     [(memq (car a) b) (minus (cdr a) b acc)]
	     [else (minus (cdr a) b (cons (car a) acc))]))))

      (define set-intersect ; O(|a|*|b|)
	(lambda (a b)
	  (if (or (set-empty? a)
		  (set-empty? b))
	      empty-set
	      (let intersect ([a (set->list a)]
			      [acc null])
		(cond
		 [(null? a) (list->set acc)]
		 [(set-memq? (car a) b) (intersect (cdr a) (cons (car a) acc))]
		 [else (intersect (cdr a) acc)])))))

      (define (set-subset? s1 s2)
	(if (eq? s1 s2)
	    #t
	    (let ([l1 (set->list s1)]
		  [l2 (set->list s2)])
	      (andmap (lambda (elt) (memq elt l2)) l1))))

      ;; -----> End list implementation <-----

      (define set-remove 
	(lambda (e s)
	  (set-minus s (make-singleton-set e))))

      (define improper-list->set
	(lambda (l)
	  (let loop ([l l][acc null])
	    (cond
	     [(null? l) (list->set acc)]
	     [(pair? l) (loop (cdr l) (cons (car l) acc))]
	     [else (list->set (cons l acc))]))))

      (define set-find
	(lambda (p s)
	  (let ([lst (set->list s)])
	    (let loop ([l lst])
	      (cond [(null? l) #f]
		    [(p (car l)) (car l)]
		    [else (loop (cdr l))])))))

      (define set-map
	(lambda (f s)
	  (list->set (map f (set->list s)))))

      (define set-filter
	(lambda (f s)
	  (list->set (filter f (set->list s)))))

      (define symbol-append
	(lambda s
	  (let loop ([str ""] [s s])
	    (if (null? s)
		(string->symbol str)
		(loop (string-append str (symbol->string (car s))) (cdr s))))))

      (define (remove-duplicates elts)
	(if (null? elts)
	    '()
	    (if (memq (car elts) (cdr elts))
		(remove-duplicates (cdr elts))
		(cons (car elts) (remove-duplicates (cdr elts))))))

					; end binder set ops

      (define compiler:formals->arity
	(lambda (f)
	  (let ([L (length (zodiac:arglist-vars f))])
	    (cond
	     [(zodiac:sym-arglist? f) (values 0 -1)]
	     [(zodiac:list-arglist? f) (values L L)]
	     [(zodiac:ilist-arglist? f) (values (- L 1) -1)]))))

      (define compiler:formals->arity*
	(lambda (fs)
	  (cond
	   [(null? fs) (values -1 0)]
	   [(null? (cdr fs)) (compiler:formals->arity (car fs))]
	   [else (let-values ([(a- a+) (compiler:formals->arity (car fs))]
			      [(b- b+) (compiler:formals->arity* (cdr fs))])
		   (values (min a- b-)
			   (if (or (negative? b+) (negative? a+))
			       -1
			       (max a+ b+))))])))
      
      (define compiler:gensym gensym)
      (define compiler:label-number 0)
      (define (compiler:reset-label-number!)
	(set! compiler:label-number 0))
      (define compiler:genlabel
	(lambda ()
	  (begin0 compiler:label-number
		  (set! compiler:label-number (add1 compiler:label-number)))))
      (define (compiler:get-label-number) compiler:label-number)

      (define re:bad-char (regexp "[][#+-.*/<=>!?:$%_&~^@;^(){}|\\,~\"`' \000-\040]"))
      (define re:starts-with-number (regexp "^[0-9]"))

      (define (compiler:clean-string s)
	(let ([s (regexp-replace* re:bad-char s "_")])
	  (if (regexp-match re:starts-with-number s)
	      (string-append "_" s)
	      s)))

      (define (protect-comment s)
	(string-append
	 (regexp-replace* "[*]/"
			  (regexp-replace* "/[*]" s "-")
			  "-")
	 " "))

      (define (global-defined-value* v)
	(and v (namespace-variable-value v))))))
