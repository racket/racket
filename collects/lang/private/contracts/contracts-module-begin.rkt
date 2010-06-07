#lang scheme/base

; Once upon a time, there were three different variants.  Preserve the
; ability to do this.
(provide (rename-out (module-begin beginner-module-begin)
		     (module-begin intermediate-module-begin)
		     (module-begin advanced-module-begin)))

(require lang/private/contracts/contract-syntax)

(require (for-syntax scheme/base)
	 (for-syntax mzlib/list)
	 (for-syntax syntax/boundmap)
	 (for-syntax syntax/kerncase))

(define-syntax (print-results stx)
  (syntax-case stx ()
    ((_ expr)
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
      'transparent))
    ((_ expr) #'expr)))

(define (do-print-results . vs)
  (for-each (current-print) vs)
  ;; Returning 0 values avoids any further result printing
  ;; (even if void values are printed)
  (values))
  
(define-syntaxes (module-begin module-continue)
  (let ()
    ;; takes a list of syntax objects (the result of syntax-e) and returns all the syntax objects that correspond to
    ;; a contract declaration. Syntax: (: id contract)
    (define extract-contracts
      (lambda (lostx) 
	(let* ((table (make-bound-identifier-mapping))
	       (non-contracts
		(filter (lambda (maybe)
			  (syntax-case maybe (:)
			    ((: ?id ?cnt)
			     (identifier? #'id)
			     (begin
			       (when (bound-identifier-mapping-get table #'?id (lambda () #f))
				 (raise-syntax-error #f
						     "Second contract declaraton for the same name."
						     maybe))
			       (bound-identifier-mapping-put! table #'?id #'?cnt)
			       #f))
			    ((: ?id)
			     (raise-syntax-error 'contracts "Contract declaration is missing a contract." maybe))
			    ((: ?id ?cnt ?stuff0 ?stuff1 ...)
			     (raise-syntax-error 'contracts "The : form expects a name and a contract; there is more."
						 (syntax/loc #'?stuff0
							     (?stuff0 ?stuff1 ...))))
			    (_ #t)))
			lostx)))
	  (values table non-contracts))))

    (define local-expand-stop-list 
      (append (list #': #'define-contract
		    #'#%require #'#%provide)
	      (kernel-form-identifier-list)))
	
    (define (expand-contract-expressions contract-table expressions)

      (let loop ((exprs expressions))

	(cond
	 ((null? exprs)
	  (bound-identifier-mapping-for-each contract-table
					     (lambda (id thing)
					       (when thing
						 (if (identifier-binding id)
						     (raise-syntax-error #f "Cannot declare a contract for a built-in form." id)
						     (raise-syntax-error #f "There is no definition for this contract declaration." id)))))
	  #'(begin))
	 (else
	  (let ((expanded (car exprs)))

	    (syntax-case expanded (begin define-values)
	      ((define-values (?id ...) ?e1)
	       (with-syntax (((?enforced ...)
			      (map (lambda (id)
				     (cond
				      ((bound-identifier-mapping-get contract-table id (lambda () #f))
				       => (lambda (cnt)
					    (bound-identifier-mapping-put! contract-table id #f) ; check for orphaned contracts
					    (with-syntax ((?id id)
							  (?cnt cnt))
					      #'(?id (contract ?cnt)))))
				      (else
				       id)))
				   (syntax->list #'(?id ...))))
			     (?rest (loop (cdr exprs))))
		 (with-syntax ((?defn
				(syntax-track-origin
				 #'(define-values/contract (?enforced ...)
				     ?e1)
				 (car exprs)
				 (car (syntax-e expanded)))))

		   (syntax/loc (car exprs)
			       (begin
				 ?defn
				 ?rest)))))
	      ((begin e1 ...)
	       (loop (append (syntax-e (syntax (e1 ...))) (cdr exprs))))
	      (else 
	       (with-syntax ((?first expanded)
			     (?rest (loop (cdr exprs))))
		 (syntax/loc (car exprs)
			     (begin
			       ?first ?rest))))))))))
    (values
     ;; module-begin
     (lambda (stx)
       (syntax-case stx ()
	 ((_ e1 ...)
	  ;; module-begin-continue takes a sequence of expanded
	  ;; exprs and a sequence of to-expand exprs; that way,
	  ;; the module-expansion machinery can be used to handle
	  ;; requires, etc.:
	  #`(#%plain-module-begin
	     (module-continue (e1 ...) () ())))))

     ;; module-continue
     (lambda (stx)
       (syntax-case stx ()
	 ((_ () (e1 ...) (defined-id ...))
	  ;; Local-expanded all body elements, lifted out requires, etc.
	  ;; Now process the result.
	  (begin
	    ;; The expansion for contracts breaks the way that beginner-define, etc.,
	    ;;  check for duplicate definitions, so we have to re-check here.
	    ;; A better strategy might be to turn every define into a define-syntax
	    ;;  to redirect the binding, and then the identifier-binding check in
	    ;;  beginner-define, etc. will work.
	    (let ((defined-ids (make-bound-identifier-mapping)))
	      (for-each (lambda (id)
			  (when (bound-identifier-mapping-get defined-ids id (lambda () #f))
			    (raise-syntax-error
			     #f
			     "There is already a definition for this name."
			     id))
			  (bound-identifier-mapping-put! defined-ids id #t))
			(reverse (syntax->list #'(defined-id ...)))))
	    ;; Now handle contracts:
	    (let ((top-level (reverse (syntax->list (syntax (e1 ...))))))
	      (let-values (((cnt-table expr-list)
			    (extract-contracts top-level)))
		(expand-contract-expressions cnt-table expr-list)))))
	 ((frm e3s e1s def-ids)
	  (let loop ((e3s #'e3s)
		     (e1s #'e1s)
		     (def-ids #'def-ids))
	    (syntax-case e3s ()
	      (()
	       #`(frm () #,e1s #,def-ids))
	      ((e2 . e3s)
	       (let ((e2 (local-expand #'e2 'module local-expand-stop-list)))
		 ;; Lift out certain forms to make them visible to the module
		 ;;  expander:
		 (syntax-case e2 (#%require #%provide
				  define-syntaxes define-values-for-syntax define-values begin
				  define-contract :)
		   ((#%require . __)
		    #`(begin #,e2 (frm e3s #,e1s #,def-ids)))
		   ((#%provide . __)
		    #`(begin #,e2 (frm e3s #,e1s #,def-ids)))
		   ((define-syntaxes (id ...) . _)
		    #`(begin #,e2 (frm e3s #,e1s (id ... . #,def-ids))))
		   ((define-values-for-syntax . _)
		    #`(begin #,e2 (frm e3s #,e1s #,def-ids)))
		   ((begin b1 ...)
		    (syntax-track-origin 
		     (loop (append (syntax->list #'(b1 ...)) #'e3s) e1s def-ids)
		     e2
		     (car (syntax-e e2))))
		   ((define-values (id ...) . _)
		    (loop #'e3s (cons e2 e1s) (append (syntax->list #'(id ...)) def-ids)))
		   ((define-contract id ctr)
		    (loop #'e3s (cons e2 e1s) def-ids))
		   ((: stuff ...)
		    (loop #'e3s (cons e2 e1s) def-ids))
		   (_
		    (loop #'e3s (cons #`(print-results #,e2) e1s) def-ids)))))))))))))
