(module contract-guts mzscheme
  (require "contract-helpers.scm"
           (lib "pretty.ss")
           (lib "list.ss"))

  (require-for-syntax "contract-helpers.scm")
  
  (provide raise-contract-error
           contract-violation->string
           coerce-contract 
           coerce/select-contract
           
           flat-contract/predicate?
           flat-contract?
           flat-contract
           flat-contract-predicate
           flat-named-contract

           build-compound-type-name
           
           and/c
           any/c
           
           contract?
           contract-name
           contract-proc
           contract-pos-proc
           contract-neg-proc
           make-pair-proj-contract
           build-flat-contract
           
           define-struct/prop
           
           contract-stronger?
           
           proj-pred? proj-get
           pos-proj-prop pos-proj-pred? pos-proj-get
           neg-proj-prop neg-proj-pred? neg-proj-get
           name-prop name-pred? name-get
           stronger-prop stronger-pred? stronger-get
           flat-prop flat-pred? flat-get
           any-curried-proj
           flat-pos-proj)
  

  ;; define-struct/prop is a define-struct-like macro that
  ;; also allows properties to be defined
  ;; it contains copied code (build-struct-names) in order to avoid
  ;; a module cycle
  (define-syntax (define-struct/prop stx)
    (let ()
      
      (syntax-case stx ()
        [(_ name (field ...) ((property value) ...))
         (andmap identifier? (syntax->list (syntax (field ...))))
         (let ([struct-names (build-struct-names (syntax name)
                                                 (syntax->list (syntax (field ...)))
                                                 #f
                                                 #t
                                                 stx)]
               [struct-names/bangers (build-struct-names (syntax name)
                                                         (syntax->list (syntax (field ...)))
                                                         #t
                                                         #f
                                                         stx)]
               [field-count/val (length (syntax->list (syntax (field ...))))])
           (with-syntax ([struct:-name (list-ref struct-names 0)]
                         [struct-maker (list-ref struct-names 1)]
                         [predicate (list-ref struct-names 2)]
                         [(count ...) (nums-up-to field-count/val)]
                         [(selectors ...) (cdddr struct-names)]
                         [(bangers ...) (cdddr struct-names/bangers)]
                         [field-count field-count/val]
                         [(field-indicies ...) (nums-up-to (length (syntax->list (syntax (field ...)))))])
             (syntax
              (begin
                (define-values (struct:-name struct-maker predicate get set)
                  (make-struct-type 'name
                                    #f ;; super
                                    field-count
                                    0 ;; auto-field-k
                                    '()
                                    (list (cons property value) ...)))
                (define selectors (make-struct-field-accessor get count 'field))
                ...
                (define bangers (make-struct-field-mutator set count 'field))
                ...))))])))
  
  (define-values (proj-prop proj-pred? raw-proj-get) 
    (make-struct-type-property 'contract-projection))
  (define-values (name-prop name-pred? name-get)
    (make-struct-type-property 'contract-name))
  (define-values (stronger-prop stronger-pred? stronger-get)
    (make-struct-type-property 'contract-stronger-than))
  (define-values (flat-prop flat-pred? flat-get)
    (make-struct-type-property 'contract-flat))
  
  (define-values (pos-proj-prop pos-proj-pred? pos-proj-get) 
    (make-struct-type-property 'contract-positive-projection))
  (define-values (neg-proj-prop neg-proj-pred? neg-proj-get) 
    (make-struct-type-property 'contract-negative-projection))
  
  (define (proj-get ctc)
    (cond
      [(proj-pred? ctc)
       (raw-proj-get ctc)]
      [(and (neg-proj-pred? ctc)
            (pos-proj-pred? ctc))
       (let ([pos-abs ((pos-proj-get ctc) ctc)]
             [neg-abs ((neg-proj-get ctc) ctc)])
         (λ (ctc)
           (λ (pos neg src-info str)
             (let ([p-proj (pos-abs pos src-info str)]
                   [n-proj (neg-abs neg src-info str)])
               (lambda (v)
                 (n-proj (p-proj v)))))))]
      [else (error 'proj-get "unknown ~e" ctc)]))
  
  ;; contract-stronger? : contract contract -> boolean
  ;; indicates if one contract is stronger (ie, likes fewer values) than another
  ;; this is not a total order.
  (define (contract-stronger? a b)
    (let ([a-ctc (coerce-contract contract-stronger? a)]
          [b-ctc (coerce-contract contract-stronger? b)])
      ((stronger-get a-ctc) a-ctc b-ctc)))
  
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
                                      struct:exn:fail:contract
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
  
  (define (default-contract-violation->string val src-info to-blame contract-sexp msg)
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
		 (format "on ~a" datum)
                 ""))])
      (string-append (format "~a~a broke the contract ~a~a; "
                             blame-src
                             to-blame
                             formatted-contract-sexp
                             specific-blame)
                     msg)))
  
  (define contract-violation->string (make-parameter default-contract-violation->string))
  
  (define (raise-contract-error val src-info blame contract-sexp fmt . args)
    (raise
     (make-exn:fail:contract2
      (string->immutable-string
       ((contract-violation->string) val 
                                     src-info 
                                     blame
                                     contract-sexp 
                                     (apply format fmt args)))
      (current-continuation-marks)
      (if src-info
          (list (make-srcloc 
                 (syntax-source src-info)
                 (syntax-line src-info)
                 (syntax-column src-info)
                 (syntax-position src-info)
                 (syntax-span src-info)))
          '()))))
  
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
  
  (define (flat-pos-proj ctc)
    (let ([predicate ((flat-get ctc) ctc)]
          [name ((name-get ctc) ctc)])
      (λ (pos src-info orig-str)
        (λ (val)
          (if (predicate val)
              val
              (raise-contract-error
               val
               src-info
               pos
               orig-str
               "expected <~a>, given: ~e"
               name
               val))))))
  
  (define (any-curried-proj ctc) any-curred-proj2)
  (define (any-curred-proj2 pos src-info orig-str) values)
  
  (define-values (make-flat-contract
                  make-pair-proj-contract)
    (let ()
      (define-struct/prop pair-proj-contract (the-name pos-proc neg-proc)
        ((pos-proj-prop (λ (ctc) (pair-proj-contract-pos-proc ctc)))
         (neg-proj-prop (λ (ctc) (pair-proj-contract-neg-proc ctc)))
         (name-prop (λ (ctc) (pair-proj-contract-the-name ctc)))
         (stronger-prop (λ (this that) 
                          (and (pair-proj-contract? that)
                               (procedure-closure-contents-eq?
                                (pair-proj-contract-pos-proc this)
                                (pair-proj-contract-pos-proc that))
                               (procedure-closure-contents-eq?
                                (pair-proj-contract-neg-proc that)
                                (pair-proj-contract-neg-proc this)))))))
      (define-struct/prop flat-contract (the-name predicate)
        ((pos-proj-prop flat-pos-proj)
         (neg-proj-prop any-curried-proj)
         (stronger-prop (λ (this that) 
                          (and (flat-contract? that)
                               (procedure-closure-contents-eq? (flat-contract-predicate this)
							       (flat-contract-predicate that)))))
         (name-prop (λ (ctc) (flat-contract-the-name ctc)))
         (flat-prop (λ (ctc) (flat-contract-predicate ctc)))))
      (values make-flat-contract
              make-pair-proj-contract)))
  
  (define (flat-contract-predicate x) 
    (unless (flat-contract? x)
      (error 'flat-contract-predicate "expected a flat contract, got ~e" x))
    ((flat-get x) x))
  (define (flat-contract? x) (flat-pred? x))
  (define (contract-name ctc) ((name-get ctc) ctc))
  (define (contract? x) (or (proj-pred? x) (pos-proj-pred? x)))
  (define (contract-proc ctc) ((proj-get ctc) ctc))
  (define (contract-pos-proc ctc) ((pos-proj-get ctc) ctc))
  (define (contract-neg-proc ctc) ((neg-proj-get ctc) ctc))
  
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
  
  (define (build-flat-contract name predicate) (make-flat-contract name predicate))
  
  ;; build-compound-type-name : (union contract symbol) ... -> (-> sexp)
  (define (build-compound-type-name . fs)
    (let loop ([subs fs])
      (cond
        [(null? subs)
         '()]
        [else (let ([sub (car subs)])
                (cond
                  [(contract? sub)
                   (let ([mk-sub-name (contract-name sub)])
                     `(,mk-sub-name ,@(loop (cdr subs))))]
                  [else `(,sub ,@(loop (cdr subs)))]))])))
  
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
              [pos-contract/procs (map contract-pos-proc contracts)]
              [neg-contract/procs (map contract-neg-proc contracts)])
         (unless (or (null? non-flats)
                     (null? (cdr non-flats)))
           (error 'and/c "expected at most one non-flat contract as argument"))
	 (make-pair-proj-contract
          (apply build-compound-type-name 'and/c contracts)
          (lambda (blame src-info orig-str)
            (let ([partial-contracts (map (lambda (contract/proc) (contract/proc blame src-info orig-str))
                                          pos-contract/procs)])
              (let loop ([ctct (car partial-contracts)]
                         [rest (cdr partial-contracts)])
                (cond
                  [(null? rest) ctct]
                  [else 
                   (let ([fst (car rest)])
                     (loop (lambda (x) (fst (ctct x)))
                           (cdr rest)))]))))
          (lambda (blame src-info orig-str)
            (let ([partial-contracts (map (lambda (contract/proc) (contract/proc blame src-info orig-str))
                                          neg-contract/procs)])
              (let loop ([ctct (car partial-contracts)]
                         [rest (cdr partial-contracts)])
                (cond
                  [(null? rest) ctct]
                  [else 
                   (let ([fst (car rest)])
                     (loop (lambda (x) (fst (ctct x)))
                           (cdr rest)))]))))))]))
  
  (define-struct/prop any/c ()
    ((pos-proj-prop any-curried-proj)
     (neg-proj-prop any-curried-proj)
     (stronger-prop (λ (this that) (any/c? that)))
     (name-prop (λ (ctc) 'any/c))
     (flat-prop (λ (ctc) (λ (x) #t)))))
  
  (define any/c (make-any/c))
  
  (define (flat-contract/predicate? pred)
    (or (flat-contract? pred)
        (and (procedure? pred)
             (procedure-arity-includes? pred 1)))))