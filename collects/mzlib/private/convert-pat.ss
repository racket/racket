(module convert-pat mzscheme
  (require "match-error.ss"
           "match-helper.ss"
           "match-expander-struct.ss")

  (require-for-template mzscheme
			"match-error.ss")
  
  (provide convert-pat handle-clauses convert-pats)
  
  ;; these functions convert the patterns from the old syntax 
  ;; to the new syntax
    
  (define (handle-clause stx)
    (syntax-case stx ()
      [(pat . rest) #`(#,(convert-pat (syntax pat)) . rest)]))
  
  (define (handle-clauses stx) (syntax-map handle-clause stx))
  
 
  (define (convert-pats stx)
    (with-syntax ([new-pats (syntax-map convert-pat stx)])
      #'new-pats))
  
    (define (imp-list? x stx)
      (define (keyword? x)
        (member (syntax-object->datum x)
                '(
                  quote
                  quasiquote
                  ?
                  =
                  and
                  or
                  not
                  $
                  set!
                  get!
                  ;unquote
                  ;unquote-splicing
                  )))
      (let/ec out
        (let loop ((x x))
          (cond ((null? x) (out #f))
                ((or (not (pair? x))
                     (and (list? x)
                          (keyword? (car x)))) 
                 (list 
                  (quasisyntax/loc stx #,x)))
                (else (cons (car x) (loop (cdr x))))))))
  
    (define (convert-quasi stx)
      (syntax-case stx (unquote quasiquote unquote-splicing)
        [,pat #`,#,(convert-pat (syntax pat))]
        [,@pat #`,@#,(convert-pat (syntax pat))]
        ((x . y)
         (quasisyntax/loc 
             stx (#,(convert-quasi (syntax x)) . #,(convert-quasi (syntax y)))))
        (pat
         (vector? (syntax-e stx))
         (quasisyntax/loc 
             stx 
           #,(list->vector (map convert-quasi 
                                (vector->list (syntax-e stx))))))
        (pat
         (box? (syntax-e stx))
         (quasisyntax/loc 
             stx #,(box (convert-quasi (unbox (syntax-e stx))))))
        (pat stx)))

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
				 "This expander only works with plt-match.")
	       (let ([introducer (make-syntax-introducer)]
		     [certifier (match-expander-certifier expander)])
		 (convert-pat/cert
		  (introducer (xformer (introducer stx)))
		  (lambda (id)
		    (certifier (cert id) #f introducer))))))]
	[p
	 (dot-dot-k? (syntax-object->datum #'p))
	 stx]
	[_ stx]
	[() #'(list)]
	['() #'(list)]
	['item stx]
	[p
	 (let ((old-pat (syntax-e #'p)))
	   (or (string? old-pat)
	       (boolean? old-pat)
	       (char? old-pat)
	       (number? old-pat)))
	 stx]
	[(? pred) #`(? #,(cert #'pred))]
	[(? pred . a)
	 (with-syntax ([pred (cert #'pred)]
		       [pats (syntax-map convert-pat #'a)])
	   #'(? pred . pats))]
	[`pat #``#,(convert-quasi #'pat)]
	[(= op pat) #`(app #,(cert #'op) #,(convert-pat #'pat))]
	[(and . pats)
	 (with-syntax ([new-pats (syntax-map convert-pat #'pats)])
	   #'(and . new-pats))]
	[(or . pats)
	 (with-syntax ([new-pats (syntax-map convert-pat #'pats)])
	   #'(or . new-pats))]
	[(not pat) #`(not #,(convert-pat #'pat))]
	[($ struct-name . fields)
	 (with-syntax ([struct-name (cert #'struct-name)]
		       [new-fields (syntax-map convert-pat #'fields)])
	   #'(struct struct-name new-fields))]
	[(get! id) (with-syntax ([id (cert #'id)])
		     #'(get! id))]
	[(set! id) (with-syntax ([id (cert #'id)])
		     #'(set! id))]
	[(quote p) stx]
	[(car-pat . cdr-pat)
	 (let ([l (imp-list? (syntax-e stx) stx)])
	   (if l #`(list-rest #,@(map convert-pat l))
	       #`(list #,@(map convert-pat (syntax-e stx)))))]
	[pt
	 (vector? (syntax-e stx))
	 (with-syntax ([new-pats (map convert-pat (vector->list (syntax-e stx)))])
	   #'(vector . new-pats))]
	[pt
	 (box? (syntax-e stx))
	 #`(box #,(convert-pat (unbox (syntax-e stx))))]
	[pt
	 (identifier? stx)
	 (cert stx)]
	[got-too-far 
	 (match:syntax-err stx "syntax error in pattern")])))
  )