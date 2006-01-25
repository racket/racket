(module contract-util mzscheme
  (require "contract-helpers.scm"
           (lib "pretty.ss")
           (lib "list.ss"))

  (provide raise-contract-error
           coerce-contract 
           coerce/select-contract
           contract?
           contract-name
           flat-contract/predicate?
           flat-contract?
           flat-contract
           flat-contract-predicate
           flat-named-contract

           build-compound-type-name
           
           and/c
           any/c
           
           contract-proc
           make-contract
           build-flat-contract
           make-flat-contract)
  
  ;; coerce/select-contract : id (union contract? procedure-arity-1) -> contract-proc
  ;; contract-proc = sym sym stx -> alpha -> alpha
  ;; returns the procedure for the contract after extracting it from the
  ;; struct. Coerces the argument to a flat contract if it is procedure, but first.
  (define-syntax (coerce/select-contract stx)
    (syntax-case stx ()
      [(_ name val)
       (syntax (coerce/select-contract/proc 'name val))]))

  (define (coerce/select-contract/proc name x)
    (cond
     [(contract? x)
      (contract-proc x)]
     [(and (procedure? x) (procedure-arity-includes? x 1))
      (contract-proc (flat-contract x))]
     [else
      (error name 
	     "expected contract or procedure of arity 1, got ~e"
	     x)]))
  
  ;; coerce-contract : id (union contract? procedure-arity-1) -> contract
  ;; contract-proc = sym sym stx -> alpha -> alpha
  ;; returns the procedure for the contract after extracting it from the
  ;; struct. Coerces the argument to a flat contract if it is procedure, but first.
  (define-syntax (coerce-contract stx)
    (syntax-case stx ()
      [(_ name val)
       (syntax (coerce-contract/proc 'name val))]))

  (define (coerce-contract/proc name x)
    (cond
     [(contract? x) x]
     [(and (procedure? x) (procedure-arity-includes? x 1))
      (flat-contract x)]
     [else
      (error name 
             "expected contract or procedure of arity 1, got ~e"
              x)]))
  
  (define-values (make-exn:fail:contract2 
                  exn:fail:contract2?
                  exn:fail:contract2-srclocs)
    (let-values ([(exn:fail:contract2 
                   make-exn:fail:contract2 
                   exn:fail:contract2?
                   get
                   set)
                  (parameterize ([current-inspector (make-inspector)])
                    (make-struct-type 'exn:fail:contract2
                                      struct:exn:fail
                                      1
                                      0
                                      #f
                                      (list (cons prop:exn:srclocs
                                                  (lambda (x)
                                                    (exn:fail:contract2-srclocs x))))))])
      (values
       make-exn:fail:contract2 
       exn:fail:contract2?
       (lambda (x) (get x 0)))))
  
  (define (raise-contract-error src-info to-blame other-party contract-sexp fmt . args)
    (let ([blame-src (src-info-as-string src-info)]
          [formatted-contract-sexp
           (let ([one-line (format "~s" contract-sexp)])
             (if (< (string-length one-line) 30)
                 (string-append one-line " ")
                 (let ([sp (open-output-string)])
                   (newline sp)
                   (parameterize ([pretty-print-print-line print-contract-liner]
                                  [pretty-print-columns 50])
                     (pretty-print contract-sexp sp))
                   (get-output-string sp))))]
	  [specific-blame
	   (let ([datum (syntax-object->datum src-info)])
	     (if (symbol? datum)
		 (format " on ~a" datum)
                 ""))])
      (raise
       (make-exn:fail:contract2
        (string->immutable-string
         (string-append (format "~a~a broke the contract ~ait had with ~a~a; "
                                blame-src
                                to-blame
                                formatted-contract-sexp
                                other-party
                                specific-blame)
                        (apply format fmt args)))
        (current-continuation-marks)
        (if src-info
            (list (make-srcloc 
                   (syntax-source src-info)
                   (syntax-line src-info)
                   (syntax-column src-info)
                   (syntax-position src-info)
                   (syntax-span src-info)))
            '())))))
  
  
  (define print-contract-liner
    (let ([default (pretty-print-print-line)])
      (λ (line port ol cols)
        (+ (default line port ol cols)
           (if line
               (begin (display "  " port)
                      2)
               0)))))
  
  ;; src-info-as-string : (union syntax #f) -> string
  (define (src-info-as-string src-info)
    (if (syntax? src-info)
        (let ([src-loc-str (build-src-loc-string src-info)])
          (if src-loc-str
              (string-append src-loc-str  ": ")
              ""))
        ""))
  
  
    ;                                                      
  ;                                                      
  ;                                                      
  ;                                                      
  ;                                                      
  ;                          ;                       ;   
  ;    ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;; 
  ;   ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;   
  ;  ;      ;     ;  ;   ;   ;    ;       ;  ;       ;   
  ;  ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;   
  ;  ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;   
  ;   ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;   
  ;    ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;; 
  ;                                                      
  ;                                                      
  ;                                                      
  
  ;; contract = (make-contract sexp
  ;;                           (sym
  ;;                            sym
  ;;                            (union syntax #f)
  ;;                            string
  ;;                            ->
  ;;                            (alpha -> alpha)))
  ;; the first arg to make-contract builds the name of the contract. The
  ;; path records how the violation occurs
  ;;
  ;; generic contract container; 
  ;; the first arg to proc is a symbol representing the name of the positive blame
  ;; the second arg to proc is the symbol representing the name of the negative blame
  ;; the third argument to proc is the src-info.
  ;; the fourth argumet is a textual representation of the original contract
  ;;
  ;; the argument to the result function is the value to test.
  ;; (the result function is the projection)
  ;;  
  (define-values (make-flat-contract
                  flat-contract-predicate
                  flat-contract?
                  
                  make-contract
                  contract-name
                  contract-proc
                  contract?)
    (let ()
      (define-struct contract (name proc))
      (define-struct (flat-contract contract) (predicate))
      (values make-flat-contract
              flat-contract-predicate
              flat-contract?
              
              make-contract
              contract-name
              contract-proc
              contract?)))
  
  
    (define (flat-contract predicate)
    (unless (and (procedure? predicate)
                 (procedure-arity-includes? predicate 1))
      (error 'flat-contract
             "expected procedure of one argument as argument, given ~e"
             predicate))
    (let ([pname (object-name predicate)])
      (if pname
          (flat-named-contract pname predicate)
          (flat-named-contract '??? predicate))))
  
  (define (flat-named-contract name predicate)
    (unless (and (procedure? predicate)
                 (procedure-arity-includes? predicate 1))
      (error 'flat-named-contract
             "expected procedure of one argument as second argument, given: ~e, fst arg ~e"
             predicate name))
    (build-flat-contract name predicate))
  
  (define (build-flat-contract name predicate)
    (make-flat-contract
     name
     (lambda (pos neg src-info orig-str) 
       (lambda (val)
         (if (predicate val)
             val
             (raise-contract-error
              src-info
              pos
              neg
              orig-str
              "expected <~a>, given: ~e"
              name
              val))))
     predicate))
  
  ;; build-compound-type-name : (union contract symbol) ... -> (-> sexp)
  (define (build-compound-type-name . fs)
    (let loop ([subs fs]
               [i 0])
      (cond
        [(null? subs)
         '()]
        [else (let ([sub (car subs)])
                (cond
                  [(contract? sub)
                   (let ([mk-sub-name (contract-name sub)])
                     `(,mk-sub-name ,@(loop (cdr subs) (+ i 1))))]
                  [else `(,sub ,@(loop (cdr subs) i))]))])))
  
  (define (and/c . fs)
    (for-each
     (lambda (x) 
       (unless (or (contract? x)
                   (and (procedure? x)
                        (procedure-arity-includes? x 1)))
         (error 'and/c "expected procedures of arity 1 or <contract>s, given: ~e" x)))
     fs)
    (cond
      [(null? fs) any/c]
      [(andmap flat-contract/predicate? fs)
       (let* ([to-predicate
	       (lambda (x)
		 (if (flat-contract? x)
		     (flat-contract-predicate x)
		     x))]
	      [contracts (map (lambda (x) (if (contract? x) x (flat-contract x))) fs)]
              [pred
	       (let loop ([pred (to-predicate (car fs))]
			  [preds (cdr fs)])
		 (cond
		   [(null? preds) pred]
		   [else
		    (let* ([fst (to-predicate (car preds))])
		      (loop (let ([and/c-contract? (lambda (x) (and (pred x) (fst x)))])
                              and/c-contract?)
			    (cdr preds)))]))])
	 (flat-named-contract (apply build-compound-type-name 'and/c contracts) pred))]
      [else
       (let* ([non-flats (filter (λ (x) 
                                   (and (not (procedure? x))
                                        (not (flat-contract? x))))
                                 fs)]
              [contracts (map (lambda (x) (if (contract? x) x (flat-contract x))) fs)]
              [contract/procs (map contract-proc contracts)])
         (unless (or (null? non-flats)
                     (null? (cdr non-flats)))
           (error 'and/c "expected at most one non-flat contract as argument"))
	 (make-contract
          (apply build-compound-type-name 'and/c contracts)
          (lambda (pos neg src-info orig-str)
            (let ([partial-contracts (map (lambda (contract/proc) (contract/proc pos neg src-info orig-str))
                                          contract/procs)])
              (let loop ([ctct (car partial-contracts)]
                         [rest (cdr partial-contracts)])
                (cond
                  [(null? rest) ctct]
                  [else 
                   (let ([fst (car rest)])
                     (loop (lambda (x) (fst (ctct x)))
                           (cdr rest)))]))))))]))
  
  (define any/c
    (make-flat-contract
     'any/c
     (lambda (pos neg src-info orig-str) (lambda (val) val))
     (lambda (x) #t)))
  
  (define (flat-contract/predicate? pred)
    (or (flat-contract? pred)
        (and (procedure? pred)
             (procedure-arity-includes? pred 1)))))