(module contracts-module-begin mzscheme
  
  (require "contracts.ss")
  
  (require-for-syntax mzlib/list
		      syntax/boundmap
                      syntax/kerncase)
  
  (provide beginner-module-begin intermediate-module-begin advanced-module-begin)

  (define-syntax (print-results stx)
    (syntax-case stx ()
      [(_ expr)
       (not (or (syntax-property #'expr 'stepper-hide-completed)
                (syntax-property #'expr 'stepper-skip-completely)
                (syntax-property #'expr 'test-call)))
       (syntax-property
        (syntax-property
         #'(#%app call-with-values (lambda () expr)
                  do-print-results)
         'stepper-skipto 
         '(syntax-e cdr cdr car syntax-e cdr cdr car))
        'certify-mode
        'transparent)]
      [(_ expr) #'expr]))

  (define (do-print-results . vs)
    (for-each (current-print) vs)
    ;; Returning 0 values avoids any further result printing
    ;; (even if void values are printed)
    (values))
  
  (define-syntaxes (beginner-module-begin intermediate-module-begin advanced-module-begin
					  beginner-continue intermediate-continue advanced-continue)
    (let ()
      (define (parse-contracts language-level-contract language-level-define-data
			       module-begin-continue-id)
	;; takes a list of syntax objects (the result of syntax-e) and returns all the syntax objects that correspond to
	;; a contract declaration. Syntax: (contract function-name (domain ... -> range))
	(define extract-contracts
	  (lambda (lostx) 
	    (filter contract-stx? lostx)))
	
        ;; negate previous
	(define extract-not-contracts
	  (lambda (stx-list) 
	    (filter (lambda (x) (not (contract-stx? x))) stx-list)))
	
        ;; predicate: is this syntax object a contract expr?
	(define contract-stx?
	  (lambda (stx) 
	    (syntax-case stx () 
	      [(contract function cnt) 
	       (and (identifier? #'contract)
		    (module-identifier=? #'contract language-level-contract))]
	      [_ #f])))
	
        ;; pred: is this syntax obj a define-data?
	(define define-data-stx?
	  (lambda (stx)
	    (syntax-case stx ()
	      [(define-data name e1 e2 ...)
	       (and (identifier? #'define-data)
		    (module-identifier=? #'define-data language-level-define-data))]
	      [_ #f])))
	
	;; takes a list of contract stx and a definitions stx and tells you if there is a contract defined for this function
	(define contract-defined?
	  (lambda (cnt-list item)
	    (cond 
	     [(null? cnt-list) #f]
	     [(fn=? (get-function-from-contract (car cnt-list)) (get-function-from-def item)) #t]
	     [else (contract-defined? (cdr cnt-list) item)])))
	
	;; returns the name of the function in a given contract-syntax
	(define get-function-from-contract
	  (lambda (stx) 
	    (if (contract-stx? stx)
		(syntax-case stx () 
		  [(contract function cnt ...) (syntax function)]
                  [_ (raise-syntax-error 'contract "internal error.1")])
		(raise-syntax-error 'contract "this is not a valid contract" stx))))  
	
	;; used to match up contract definitions with function definitions
					; should just be bound-identifier=?, but since beginner does some funny things
					; with hygiene, we have to do this
	(define (fn=? a b)
	  (string=? (symbol->string (syntax-object->datum a)) 
		    (symbol->string (syntax-object->datum b))))
	
	;; search in the cnt-list for the contract that matches the given definition
	(define get-contract
	  (lambda (cnt-list def-stx)
	    (cond
	     [(null? cnt-list) (error 'get-contract "contract not found")]
	     [(fn=? (get-function-from-contract (car cnt-list)) (get-function-from-def def-stx)) (car cnt-list)]
	     [else (get-contract (cdr cnt-list) def-stx)])))
	
	;; returns the name of the function in a given definition-syntax
	(define get-function-from-def
	  (lambda (stx) 
	    (if (definition-stx? stx)
		(syntax-case stx (begin define-values define-syntaxes) 
		  [(define-values (f) e1 ...) (syntax f)]
                  [_ (raise-syntax-error 'contract "internal error.2")])
		(raise-syntax-error 'defs "this is not a valid definition" stx))))
	
	;; given a syntax object, tells you whether or not this is a definition.
	(define definition-stx? 
	  (lambda (stx) 
	    (syntax-case stx (begin define-values)
	      [(define-values (f) e1 ...) #t]
	      [_ #f])))
	
	;;transform-definiton
	(define (transform-definition def)
	  (syntax-case def (define-values)
	    [(define-values (func) exp)
	     (with-syntax ([new-name (rename-func def)]
			   [expr-infname (syntax-property (syntax exp) 'inferred-name 
							  (syntax-object->datum (syntax func)))])
	       (syntax/loc def (define-values (new-name) expr-infname)))]
            [_ (raise-syntax-error 'contract "internal error.3")]))
	
	(define (rename-func def)
	  (let ([name (get-function-from-def def)])
	    (syntax-case def (define-values)
	      [(define-values (f) e1)
	       (datum->syntax-object (syntax f) 
				     (string->symbol (string-append (symbol->string (syntax-object->datum name)) "-con")))]
              [_ (raise-syntax-error 'contract "internal error.4")])))
	

	;; transform-contract : syntax syntax -> syntax
	;; takes in two syntax objects: one representing a contract, and another representing a definition, 
	;; returns a syntax object that returns the correct language level contract wrapping
	(define transform-contract
	  (lambda (language-level-contract cnt-stx def-stx)
	    (syntax-case cnt-stx ()
	      [(contract function cnt) 
	       (with-syntax ([ll-contract language-level-contract]
			     [name-to-bind (get-function-from-def def-stx)]
			     [func-to-wrap (rename-func def-stx)])
		 (syntax/loc cnt-stx (ll-contract 'name-to-bind 'func-to-wrap cnt)))]
              [_ (raise-syntax-error 'contract "internal error.5")])))
	
	(define local-expand-stop-list 
          (append (list #'contract #'#%require #'#%provide language-level-define-data)
                  (kernel-form-identifier-list)))

	;; parse-contract-expressions 
	;; takes in a list of top level expressions and a list of contracts, and outputs the correct transformation. 
	;; 1. expand until we find a definition or a contract
	;; 2. if its a definition, and it has a contract, transform and output
	;; 3. else just output it
	(define (parse-contract-expressions ll-contract ll-define-data contract-list expressions)

	  (let loop ([cnt-list contract-list]
		     [exprs expressions])

	    (cond
              [(null? exprs)
               (if (null? cnt-list)
                   (syntax (begin ))
                   (raise-syntax-error 'contracts "this contract has no corresponding def" (car cnt-list)))]
              [else
               (let ([expanded (car exprs)])

                 (syntax-case expanded (begin define-values)
                   [(define-values (func) e1 ...)
                    (contract-defined? cnt-list expanded)
                    (let ([cnt (get-contract cnt-list expanded)])
                      (quasisyntax/loc (car exprs)
                        (begin
                          #,(transform-definition expanded)
                          #,(transform-contract ll-contract cnt expanded)
                          #,(loop (remove cnt cnt-list) (cdr exprs)))))]
                   [(define-data name c1 c2 ...)
                    (and (identifier? #'name)
                         (define-data-stx? expanded))
                    (quasisyntax/loc (car exprs)
                      (begin
                        (#,ll-define-data name c1 c2 ...)
                        #,(loop cnt-list (cdr exprs))))]
                   [(begin e1 ...)
                    (loop cnt-list (append (syntax-e (syntax (e1 ...))) (cdr exprs)))]
		   [_else 
		    (quasisyntax/loc (car exprs)
		      (begin
			#,(car exprs)
			#,(loop cnt-list (cdr exprs))))]))])))
	
	;; contract transformations!
	;; this is the macro, abstracted over which language level we are using. 
	;; parse-contracts : 
	;; given transformers that handle the actual contract parsing (depends on language level.. see contracts.scm and define-data.scm
	;; this returns a big wrapper macro that translates calls to 
	;; (contract f (number -> number)) (define f ...)
	;; ====>>>> (lang-lvl-contract f (number -> number) ...)
	;; where ll-contract is either beginner-contract, intermediate-contract, or advanced-contract
	;; and (define-data name ....) to (lang-lvl-define-data name ...)

	(values
	 ;; module-begin (for a specific language:)
	 (lambda (stx)
	   (syntax-case stx ()
	     [(_ e1 ...)
	      ;; module-begin-continue takes a sequence of expanded
	      ;; exprs and a sequence of to-expand exprs; that way,
	      ;; the module-expansion machinery can be used to handle
	      ;; requires, etc.:
	      #`(#%plain-module-begin
		 (#,module-begin-continue-id (e1 ...) () ()))]))

	 ;; module-continue (for a specific language:)
	 (lambda (stx)
	   (syntax-case stx ()
	     [(_ () (e1 ...) (defined-id ...))
	      ;; Local-expanded all body elements, lifted out requires, etc.
	      ;; Now process the result.
	      (begin
		;; The expansion for contracts breaks the way that beginner-define, etc.,
		;;  check for duplicate definitions, so we have to re-check here.
		;; A better strategy might be to turn every define into a define-syntax
		;;  to redirect the binding, and then the identifier-binding check in
		;;  beginner-define, etc. will work.
		(let ([defined-ids (make-bound-identifier-mapping)])
		  (for-each (lambda (id)
			      (when (bound-identifier-mapping-get defined-ids id (lambda () #f))
				(raise-syntax-error
				 #f
				 "this name was defined previously and cannot be re-defined"
				 id))
			      (bound-identifier-mapping-put! defined-ids id #t))
			    (reverse (syntax->list #'(defined-id ...)))))
		;; Now handle contracts:
		(let* ([top-level (reverse (syntax->list (syntax (e1 ...))))]
		       [cnt-list (extract-contracts top-level)]
		       [expr-list (extract-not-contracts top-level)])
		  (parse-contract-expressions language-level-contract
					      language-level-define-data
					      cnt-list
					      expr-list)))]
	     [(frm e3s e1s def-ids)
              (let loop ([e3s #'e3s]
                         [e1s #'e1s]
                         [def-ids #'def-ids])
                (syntax-case e3s ()
                  [()
                   #`(frm () #,e1s #,def-ids)]
                  [(e2 . e3s)
                   (let ([e2 (local-expand #'e2 'module local-expand-stop-list)])
                     ;; Lift out certain forms to make them visible to the module
                     ;;  expander:
                     (syntax-case e2 (#%require #%provide define-syntaxes define-values-for-syntax define-values begin)
                       [(#%require . __)
                        #`(begin #,e2 (frm e3s #,e1s #,def-ids))]
                       [(#%provide . __)
                        #`(begin #,e2 (frm e3s #,e1s #,def-ids))]
                       [(define-syntaxes (id ...) . _)
                        #`(begin #,e2 (frm e3s #,e1s (id ... . #,def-ids)))]
                       [(define-values-for-syntax . _)
                        #`(begin #,e2 (frm e3s #,e1s #,def-ids))]
                       [(begin b1 ...)
                        (syntax-track-origin 
                         (loop (append (syntax->list #'(b1 ...)) #'e3s) e1s def-ids)
                         e2
                         (car (syntax-e e2)))]
                       [(define-values (id ...) . _)
                        (loop #'e3s (cons e2 e1s) (append (syntax->list #'(id ...)) def-ids))]
                       [_
                        (loop #'e3s (cons #`(print-results #,e2) e1s) def-ids)]))]))]))))
      
      (define-values (parse-beginner-contract/func continue-beginner-contract/func)
        (parse-contracts #'beginner-contract #'beginner-define-data #'beginner-continue))
      (define-values (parse-intermediate-contract/func continue-intermediate-contract/func)
        (parse-contracts #'intermediate-contract  #'intermediate-define-data #'intermediate-continue))
      (define-values (parse-advanced-contract/func continue-advanced-contract/func)
        (parse-contracts #'advanced-contract #'advanced-define-data #'advanced-continue))
    
      (values parse-beginner-contract/func
              parse-intermediate-contract/func
              parse-advanced-contract/func
	      continue-beginner-contract/func
              continue-intermediate-contract/func
              continue-advanced-contract/func))))
