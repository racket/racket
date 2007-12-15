(module contract-arr-checks mzscheme
  (provide (all-defined))
  (require (lib "list.ss")
           "contract-guts.ss")

  (define empty-case-lambda/c
    (flat-named-contract '(case->)
                         (λ (x) (and (procedure? x) (null? (procedure-arity x))))))
  
  ;; ----------------------------------------
  ;; Checks and error functions used in macro expansions
  
  ;; procedure-accepts-and-more? : procedure number -> boolean
  ;; returns #t if val accepts dom-length arguments and
  ;; any number of arguments more than dom-length. 
  ;; returns #f otherwise.
  (define (procedure-accepts-and-more? val dom-length)
    (let ([arity (procedure-arity val)])
      (cond
        [(number? arity) #f]
        [(arity-at-least? arity)
         (<= (arity-at-least-value arity) dom-length)]
        [else
         (let ([min-at-least (let loop ([ars arity]
                                        [acc #f])
                               (cond
                                 [(null? ars) acc]
                                 [else (let ([ar (car ars)])
                                         (cond
                                           [(arity-at-least? ar)
                                            (if (and acc
                                                     (< acc (arity-at-least-value ar)))
                                                (loop (cdr ars) acc)
                                                (loop (cdr ars) (arity-at-least-value ar)))]
                                           [(number? ar)
                                            (loop (cdr ars) acc)]))]))])
           (and min-at-least
                (begin
                  (let loop ([counts (sort (filter number? arity) >=)])
                    (unless (null? counts)
                      (let ([count (car counts)])
                        (cond
                          [(= (+ count 1) min-at-least)
                           (set! min-at-least count)
                           (loop (cdr counts))]
                          [(< count min-at-least)
                           (void)]
                          [else (loop (cdr counts))]))))
                  (<= min-at-least dom-length))))])))
  
  (define (check->* f arity-count)
    (unless (procedure? f)
      (error 'object-contract "expected last argument of ->d* to be a procedure, got ~e" f))
    (unless (procedure-arity-includes? f arity-count)
      (error 'object-contract 
	     "expected last argument of ->d* to be a procedure that accepts ~a arguments, got ~e"
	     arity-count
	     f)))

  (define (check->*/more f arity-count)
    (unless (procedure? f)
      (error 'object-contract "expected last argument of ->d* to be a procedure, got ~e" f))
    (unless (procedure-accepts-and-more? f arity-count)
      (error 'object-contract 
	     "expected last argument of ->d* to be a procedure that accepts ~a argument~a and arbitrarily many more, got ~e"
	     arity-count
             (if (= 1 arity-count) "" "s")
	     f)))


  (define (check-pre-expr->pp/h val pre-expr src-info blame orig-str)
    (unless pre-expr
      (raise-contract-error val
                            src-info
                            blame
                            orig-str
                            "pre-condition expression failure")))
  
  (define (check-post-expr->pp/h val post-expr src-info blame orig-str)
    (unless post-expr
      (raise-contract-error val
                            src-info
                            blame
                            orig-str
                            "post-condition expression failure")))
  
  (define (check-procedure val dom-length src-info blame orig-str)
    (unless (and (procedure? val)
		 (procedure-arity-includes? val dom-length))
      (raise-contract-error
       val
       src-info
       blame
       orig-str
       "expected a procedure that accepts ~a arguments, given: ~e"
       dom-length
       val)))
  
  (define ((check-procedure? arity) val)
    (and (procedure? val)
         (procedure-arity-includes? val arity)))
  
  (define ((check-procedure/more? arity) val)
    (and (procedure? val)
         (procedure-accepts-and-more? val arity)))

  (define (check-procedure/kind val arity kind-of-thing src-info blame orig-str)
    (unless (procedure? val)
      (raise-contract-error val
                            src-info
			    blame
                            orig-str
			    "expected a procedure, got ~e"
			    val))
    (unless (procedure-arity-includes? val arity)
      (raise-contract-error val
                            src-info
			    blame
                            orig-str
			    "expected a ~a of arity ~a (not arity ~a), got  ~e"
			    kind-of-thing
			    arity
			    (procedure-arity val)
			    val)))

  (define (check-procedure/more/kind val arity kind-of-thing src-info blame orig-str)
    (unless (procedure? val)
      (raise-contract-error val
                            src-info
			    blame
			    orig-str
			    "expected a procedure, got ~e"
			    val))
    (unless (procedure-accepts-and-more? val arity)
      (raise-contract-error val
                            src-info
			    blame
                            orig-str
			    "expected a ~a that accepts ~a arguments and aribtrarily more (not arity ~a), got  ~e"
			    kind-of-thing
			    arity
			    (procedure-arity val)
			    val)))

  (define (check-procedure/more val dom-length src-info blame orig-str)
    (unless (and (procedure? val)
		 (procedure-accepts-and-more? val dom-length))
      (raise-contract-error
       val
       src-info
       blame
       orig-str
       "expected a procedure that accepts ~a arguments and any number of arguments larger than ~a, given: ~e"
       dom-length
       dom-length
       val)))


  (define (check-rng-procedure who rng-x arity)
    (unless (and (procedure? rng-x)
		 (procedure-arity-includes? rng-x arity))
      (error who "expected range position to be a procedure that accepts ~a arguments, given: ~e"
	     arity
	     rng-x)))

  (define (check-rng-procedure/more rng-mk-x arity)
    (unless (and (procedure? rng-mk-x)
		 (procedure-accepts-and-more? rng-mk-x arity))
      (error '->d* "expected range position to be a procedure that accepts ~a arguments and arbitrarily many more, given: ~e"
	     arity 
	     rng-mk-x)))

  (define (check-rng-lengths results rng-contracts)
    (unless (= (length results) (length rng-contracts))
      (error '->d* 
	     "expected range contract contructor and function to have the same number of values, given: ~a and ~a respectively" 
	     (length results) (length rng-contracts))))

  #|

  test cases for procedure-accepts-and-more?

  (and (procedure-accepts-and-more? (lambda (x . y) 1) 3)
       (procedure-accepts-and-more? (lambda (x . y) 1) 2)
       (procedure-accepts-and-more? (lambda (x . y) 1) 1)
       (not (procedure-accepts-and-more? (lambda (x . y) 1) 0))
       
       (procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 3)
       (procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 2)
       (procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 1)
       (not (procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 0))
       
       (procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 2)
       (procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 1)
       (not (procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 0)))
  
  |#
)
