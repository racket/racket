#| tests are at plt/collects/tests/mzscheme/
collects/tests/mzscheme/beginner.ss
                    .../beginner-abbr.ss
                    .../intermediate.ss
                    .../intermediate-lam.ss
                    .../advanced.ss

Each one has to run separately, since they mangle the top-level
namespace.
|#
(module teachprims mzscheme

  (require mzlib/list
           mzlib/etc)
  
  (define-syntax (define-teach stx)
    (syntax-case stx ()
      [(_ level id expr)
       (with-syntax ([level-id (datum->syntax-object
				 (syntax id)
				 (string->symbol
				   (format "~a-~a"
				     (syntax-object->datum (syntax level))
				     (syntax-object->datum (syntax id))))
				 (syntax id))])
	 (syntax (define level-id
		   (let ([id expr])
		     id))))]))

  (define-teach beginner list?
    (lambda (x)
      (or (null? x) (pair? x))))
		    
  ;; Don't need this anymore, since we just check for pairs:
  #;
  (define cyclic-list?
    (lambda (l)
      (or (list? l)
	  (and (pair? l)
	    (let loop ([hare (cdr l)][turtle l])
	      (cond
		[(eq? hare turtle) #t]
		[(not (pair? hare)) #f]
		[(eq? (cdr hare) turtle) #t]
		[(not (pair? (cdr hare))) #f]
		[else (loop (cddr hare) (cdr turtle))]))))))

  (define cyclic-list? beginner-list?)

  (define (build-arg-list args)
    (let loop ([args args][n 0])
      (cond
	[(null? args) ""]
	[(= n 5) " ..."]
	[else
	  (format " ~e~a" (car args) (loop (cdr args) (add1 n)))])))

  (define (mk-check-second ok? type)
    (lambda (prim-name a b)
      (unless (ok? b)
	(raise
         (make-exn:fail:contract
          (format "~a: second argument must be of type <~a>, given ~e and ~e"
                  prim-name type
                  a b)
          (current-continuation-marks))))))

  (define check-second 
    (mk-check-second beginner-list? "list"))

  (define check-second/cycle
    (mk-check-second cyclic-list? "list or cyclic list"))

  (define (mk-check-last ok? type)
    (lambda (prim-name args)
      (let loop ([l args])
	(cond
	  [(null? l) (void)]
	  [(null? (cdr l))
	   (let ([last (car l)])
	     (unless (ok? last)
	       (raise
                (make-exn:fail:contract
                 (format "~a: last argument must be of type <~a>, given ~e; other args:~a"
                         prim-name type
                         last
			 ;; all-but-last:
                         (build-arg-list
                          (let loop ([args args])
                            (cond
                              [(null? (cdr args)) null]
                              [else (cons (car args) (loop (cdr args)))]))))
                 (current-continuation-marks)))))]
	  [else (loop (cdr l))]))))

  (define check-last
    (mk-check-last beginner-list? "list"))

  (define check-last/cycle
    (mk-check-last cyclic-list? "list or cyclic list"))

  (define (check-three a b c prim-name ok1? 1type ok2? 2type ok3? 3type)
    (let ([bad
           (lambda (v which type)
             (raise
              (make-exn:fail:contract
               (format "~a: ~a argument must be of type <~a>, given ~e, ~e, and ~e"
                       prim-name which type
                       a b c)
               (current-continuation-marks))))])
      (unless (ok1? a) (bad a "first"  1type))
      (unless (ok2? b) (bad b "second" 2type))
      (unless (ok3? c) (bad c "third"  3type))))

  (define (positive-real? v)
    (and (real? v) (>= v 0)))

  (define (false? v) (eq? v #f))

  (define-teach beginner not
    (lambda (a)
      (unless (boolean? a)
	(raise
	  (make-exn:fail:contract
	    (format "not: expected either true or false; given ~e" a)
	    (current-continuation-marks))))
      (not a)))

  (define-teach beginner +
    (lambda (a b . args)
      (apply + a b args)))

  (define-teach beginner /
    (lambda (a b . args)
      (apply / a b args)))
  
  (define-teach beginner *
    (lambda (a b . args)
      (apply * a b args)))
  
  (define-teach beginner member 
    (lambda (a b)
      (check-second 'member a b)
      (not (boolean? (member a b)))))

  (define-teach beginner cons 
    (lambda (a b)
      (check-second 'cons a b)
      (cons a b)))
  
  (define-teach beginner list*
    (lambda x
      (check-last 'list* x)
      (apply list* x)))
  
  (define-teach beginner append
    (lambda (a b . x)
      (check-last 'append (cons a (cons b x)))
      (apply append a b x)))
  
  (define-teach beginner error
    (lambda (sym str)
      (unless (and (symbol? sym)
		(string? str))
	(raise
         (make-exn:fail:contract
          (format "error: expected a symbol and a string, got ~e and ~e"
                  sym str)
          (current-continuation-marks))))
      (error sym "~a" str)))

  (define-teach beginner struct?
    (lambda (x)
      (not (or (number? x)
	       (boolean? x)
	       (pair? x)
	       (symbol? x)
	       (string? x)
	       (procedure? x)
	       (vector? x)
	       (char? x)
	       (port? x)
	       (eof-object? x)
	       (void? x)))))

  (define-teach beginner exit
    (lambda () (exit)))
  
  (define (tequal? a b epsilon)
    (let* ([ht (make-hash-table)]
           [union-find (lambda (a)
                         (let loop ([prev a]
                                    [prev-prev a])
                           (let ([v (hash-table-get ht prev #f)])
                             (if v
                                 (loop v prev)
                                 (begin
                                   (let loop ([a a])
                                     (unless (eq? a prev-prev)
                                       (let ([v (hash-table-get ht a)])
                                         (hash-table-put! ht a prev)
                                         (loop v))))
                                   prev)))))]
           [union-equal!? (lambda (a b)
                            (let ([a (union-find a)]
                                  [b (union-find b)])
                              (if (eq? a b)
                                  #t
                                  (begin
                                    (hash-table-put! ht b a)
                                    #f))))])
      (let ? ([a a][b b])
        (cond
         [(real? a)
          (and (real? b)
               (beginner-=~ a b epsilon))]
         [(union-equal!? a b) #t]
         [else (equal?/recur a b ?)]))))

  (define-teach beginner equal?
    (lambda (a b)
      (equal? a b)))

  (define-teach beginner =~
    (lambda (a b c)
      (check-three a b c '=~ real? 'real real? 'real positive-real? 'non-negative-real)
      (<= (- a c) b (+ a c))))

  (define-teach beginner equal~?
    (lambda (a b c)
      (check-three a b c 'equal~? values 'any values 'any positive-real? 'non-negative-real)
      (tequal? a b c)))

  (define (qcheck quicksort fmt-str . x)
    (raise
      (make-exn:fail:contract
       (string-append (format "~a : " quicksort) (apply format fmt-str x))
       (current-continuation-marks))))

  (define (do-sort l cmp? name)
    (unless (beginner-list? l) 
      (qcheck name "first argument must be of type <list>, given ~e" l))
    (unless (and (procedure? cmp?) (procedure-arity-includes? cmp? 2))
      (qcheck name "second argument must be a <procedure> that accepts two arguments, given ~e" cmp?))
    (sort l (lambda (x y) 
              (define r (cmp? x y))
              (unless (boolean? r)
                (qcheck name "the results of the procedure argument must be of type <boolean>, produced ~e" r))
              r)))
    
  (define-teach intermediate quicksort
    (lambda (l cmp?)
      (do-sort l cmp? 'quicksort)))
  (define-teach intermediate sort
    (lambda (l cmp?)
      (do-sort l cmp? 'sort)))

  (define-teach intermediate foldr
    (lambda (f e l)
      (unless (and (procedure? f) (procedure-arity-includes? f 2))
        (qcheck 'foldr "first argument must be a <procedure> that accepts two arguments, given ~e" f))
      (unless (beginner-list? l) 
        (qcheck 'foldr "third argument must be of type <list>, given ~e" l))
      (foldr f e l)))

  (define-teach intermediate foldl
    (lambda (f e l)
      (unless (and (procedure? f) (procedure-arity-includes? f 2))
        (qcheck 'foldl "first argument must be a <procedure> that accepts two arguments, given ~e" f))
      (unless (beginner-list? l) 
        (qcheck 'foldl "third argument must be of type <list>, given ~e" l))
      (foldl f e l)))

  (define-teach intermediate build-string
    (lambda (n f)
      (unless (and (procedure? f) (procedure-arity-includes? f 1))
        (qcheck 'build-string "second argument must be a <procedure> that accepts one argument, given ~e" f))
      (unless (and (number? n) (integer? n) (>= n 0))
        (qcheck 'build-string "first argument must be of type <natural number>, given ~e" n))
      (build-string n (lambda (i)
			(define r (f i))
			(unless (char? r)
			  (qcheck 'build-string
			    "second argument must be a <procedure> that produces a <char>, given ~e, which produced ~e for ~e" f r i))
			r))))
	
			  

  (define-teach advanced cons 
    (lambda (a b)
      (check-second/cycle 'cons a b)
      (cons a b)))
  
  (define-teach advanced list*
    (lambda x
      (check-last/cycle 'list* x)
      (apply list* x)))
  
  (define-teach advanced append
    (lambda x
      (check-last/cycle 'append x)
      (apply append x)))
  
  (provide  
    false?
    beginner-not
    beginner-+
    beginner-/
    beginner-*
    beginner-list?
    beginner-member
    beginner-cons
    beginner-list*
    beginner-append
    beginner-error
    beginner-struct?
    beginner-exit
    beginner-equal?
    beginner-equal~?
    beginner-=~
    intermediate-quicksort
    intermediate-sort
    intermediate-foldr
    intermediate-foldl
    intermediate-build-string
    advanced-cons
    advanced-list*
    advanced-append
    cyclic-list?))
