(module convert-pat mzscheme
  (require "match-error.ss"
           "match-helper.ss"
           "match-expander-struct.ss"
           "observe-step.ss")
  
  (require-for-template mzscheme
			"match-error.ss")
  
  (provide convert-pat handle-clauses convert-pats)
  
  ;; these functions convert the patterns from the old syntax 
  ;; to the new syntax
  
  (define (handle-clause stx)
    (syntax-case stx ()
      [(pat . rest) (quasisyntax/loc stx (#,(convert-pat #'pat) . rest))]))
  
  (define (handle-clauses stx) (syntax-map handle-clause stx))
  
  
  (define (convert-pats stx)
    (with-syntax ([new-pats (syntax-map convert-pat stx)])
      #'new-pats))
  
  (define (imp-list? stx)
    (define datum (syntax-e stx))
    (define (keyword? x)
      (memq (syntax-object->datum x)
            '(quote quasiquote ? = and or not $ set! get!)))
    (let/ec out
      (let loop ([x datum])
        (cond [(null? x) (out #f)]
              [(or (not (pair? x))
                   (and (list? x)
                        (keyword? (car x))))
               (list 
                (quasisyntax/loc stx #,x))]
              [else (cons (car x) (loop (cdr x)))]))))
  
  (define (convert-quasi stx)
    (syntax-case stx (unquote quasiquote unquote-splicing)
      [,pat (quasisyntax/loc stx ,#,(convert-pat (syntax pat)))]
      [,@pat (quasisyntax/loc stx ,@#,(convert-pat (syntax pat)))]
      [(x . y)
       (quasisyntax/loc 
           stx (#,(convert-quasi (syntax x)) . #,(convert-quasi (syntax y))))]
      [pat
       (vector? (syntax-e stx))
       (quasisyntax/loc 
           stx 
         #,(list->vector (map convert-quasi 
                              (vector->list (syntax-e stx)))))]
      [pat
       (box? (syntax-e stx))
       (quasisyntax/loc 
           stx #,(box (convert-quasi (unbox (syntax-e stx)))))]
      [pat stx]))
  
  (define (convert-pat stx)
    (convert-pat/cert stx (lambda (x) x)))
  
  (define (convert-pat/cert stx cert)
    (let ([convert-pat (lambda (x) (convert-pat/cert x cert))])
      (syntax-case* 
	  stx
        (_ ? = and or not $ set! get! quasiquote 
           quote unquote unquote-splicing) stx-equal?
	[(expander . args)
	 (and (identifier? #'expander)
	      (match-expander? (syntax-local-value (cert #'expander) (lambda () #f))))
	 (let* ([expander (syntax-local-value (cert #'expander) (lambda () #f))]
		[xformer (match-expander-match-xform expander)])
	   (if (not xformer)
	       (match:syntax-err #'expander
				 "This expander only works with plt-match.ss.")
	       (let* ([introducer (make-syntax-introducer)]
                      [certifier (match-expander-certifier expander)]
                      [mstx (introducer stx)]
                      [mresult (xformer mstx)]
                      [result (introducer mresult)]
                      [cert* (lambda (id) (certifier (cert id) #f introducer))])
                 (observe-step stx mstx mresult result)
                 (convert-pat/cert result cert*))))]
	[p
	 (dot-dot-k? (syntax-object->datum #'p))
	 stx]
	[_ stx]
	[() (syntax/loc stx (list))]
	['() (syntax/loc stx (list))]
	['item stx]
	[p (constant-data? (syntax-e stx)) stx]
	[(? pred) (quasisyntax/loc stx (? #,(cert #'pred)))]
	[(? pred . a)
	 (with-syntax ([pred (cert #'pred)]
		       [pats (syntax-map convert-pat #'a)])
	   (syntax/loc stx (? pred . pats)))]
	[`pat (quasisyntax/loc stx `#,(convert-quasi #'pat))]
	[(= op pat) (quasisyntax/loc stx (app #,(cert #'op) #,(convert-pat #'pat)))]
	[(and . pats)
	 (with-syntax ([new-pats (syntax-map convert-pat #'pats)])
	   (syntax/loc stx (and . new-pats)))]
	[(or . pats)
	 (with-syntax ([new-pats (syntax-map convert-pat #'pats)])
	   (syntax/loc stx (or . new-pats)))]
	[(not . pats)
         (with-syntax ([new-pats (syntax-map convert-pat #'pats)])
	   (syntax/loc stx (not . new-pats)))]
	[($ struct-name . fields)
	 (with-syntax ([struct-name (cert #'struct-name)]
		       [new-fields (syntax-map convert-pat #'fields)])
	   (syntax/loc stx (struct struct-name new-fields)))]
	[(get! id) (with-syntax ([id (cert #'id)])
		     (syntax/loc stx (get! id)))]
	[(set! id) (with-syntax ([id (cert #'id)])
		     (syntax/loc stx (set! id)))]
	[(quote p) stx]
        [(car-pat . cdr-pat)
         (let ([l (imp-list? stx)])
           (if l (quasisyntax/loc stx (list-rest #,@(map convert-pat l)))
               (quasisyntax/loc stx (list #,@(syntax-map convert-pat stx)))))]
        [pt
	 (vector? (syntax-e stx))
	 (with-syntax ([new-pats (map convert-pat (vector->list (syntax-e stx)))])
	   (syntax/loc stx (vector . new-pats)))]
	[pt
	 (box? (syntax-e stx))
	 (quasisyntax/loc stx (box #,(convert-pat (unbox (syntax-e stx)))))]
	[pt
	 (identifier? stx)
	 (cert stx)]
	[got-too-far 
	 (match:syntax-err stx "syntax error in pattern")])))
  )