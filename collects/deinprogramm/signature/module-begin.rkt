#lang scheme/base

(provide module-begin)

(require deinprogramm/define-record-procedures
	 deinprogramm/signature/signature
	 deinprogramm/signature/signature-syntax)

(require (for-syntax scheme/base)
	 (for-syntax racket/list)
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
    ;; a signature declaration. Syntax: (: id signature)
    (define extract-signatures
      (lambda (lostx) 
	(let* ((table (make-bound-identifier-mapping))
	       (non-signatures
		(filter-map (lambda (maybe)
			      (syntax-case maybe (:)
				((: ?exp ?sig)
				 (not (identifier? #'?exp))
				 #'(apply-signature/blame (signature ?sig) ?exp))
				((: ?id ?sig)
				 (begin
				   (cond
				    ((bound-identifier-mapping-get table #'?id (lambda () #f))
				     => (lambda (old-sig-stx)
					  (unless (equal? (syntax->datum old-sig-stx)
							  (syntax->datum #'?sig))
					    (raise-syntax-error #f
								"Zweite Signaturdeklaration für denselben Namen."
								maybe))))
				    (else
				     (bound-identifier-mapping-put! table #'?id #'?sig)))
				   #f))
				((: ?id)
				 (raise-syntax-error #f "Bei dieser Signaturdeklaration fehlt die Signatur" maybe))
				((: ?id ?sig ?stuff0 ?stuff1 ...)
				 (raise-syntax-error #f "In der :-Form werden ein Name und eine Signatur erwartet; da steht noch mehr"
						     (syntax/loc #'?stuff0
								 (?stuff0 ?stuff1 ...))))
				(_ maybe)))
			lostx)))
	  (values table non-signatures))))

    (define local-expand-stop-list 
      (append (list #': #'define-contract
		    #'#%require #'#%provide)
	      (kernel-form-identifier-list)))
	
    (define (expand-signature-expressions signature-table expressions)

      (let loop ((exprs expressions))

	(cond
	 ((null? exprs)
	  (bound-identifier-mapping-for-each signature-table
					     (lambda (id thing)
					       (when thing
						 (if (identifier-binding id)
						     (raise-syntax-error #f "Zu einer eingebauten Form kann keine Signatur deklariert werden" id)
						     (raise-syntax-error #f "Zu dieser Signatur gibt es keine Definition" id)))))
	  #'(begin))
	 (else
	  (let ((expanded (car exprs)))

	    (syntax-case expanded (begin define-values)
	      ((define-values (?id ...) ?e1)
	       (with-syntax (((?enforced ...)
			      (map (lambda (id)
				     (cond
				      ((bound-identifier-mapping-get signature-table id (lambda () #f))
				       => (lambda (sig)
					    (bound-identifier-mapping-put! signature-table id #f) ; check for orphaned signatures
					    (with-syntax ((?id id)
							  (?sig sig))
					      #'(?id (signature ?sig)))))
				      (else
				       id)))
				   (syntax->list #'(?id ...))))
			     (?rest (loop (cdr exprs))))
		 (with-syntax ((?defn
				(syntax-track-origin
				 #'(define-values/signature (?enforced ...)
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
	    ;; The expansion for signatures breaks the way that beginner-define, etc.,
	    ;;  check for duplicate definitions, so we have to re-check here.
	    ;; A better strategy might be to turn every define into a define-syntax
	    ;;  to redirect the binding, and then the identifier-binding check in
	    ;;  beginner-define, etc. will work.
	    (let ((defined-ids (make-bound-identifier-mapping)))
	      (for-each (lambda (id)
			  (when (bound-identifier-mapping-get defined-ids id (lambda () #f))
			    (raise-syntax-error
			     #f
			     "Für diesen Namen gibt es schon eine Definition."
			     id))
			  (bound-identifier-mapping-put! defined-ids id #t))
			(reverse (syntax->list #'(defined-id ...)))))
	    ;; Now handle signatures:
	    (let ((top-level (reverse (syntax->list (syntax (e1 ...))))))
	      (let-values (((sig-table expr-list)
			    (extract-signatures top-level)))
		(expand-signature-expressions sig-table expr-list)))))
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
				  define-syntaxes begin-for-syntax define-values begin
				  define-record-procedures define-record-procedures-2
				  define-record-procedures-parametric define-record-procedures-parametric-2
				  define-contract :)
		   ((#%require . __)
		    #`(begin #,e2 (frm e3s #,e1s #,def-ids)))
		   ((#%provide . __)
		    #`(begin #,e2 (frm e3s #,e1s #,def-ids)))
		   ((define-syntaxes (id ...) . _)
		    #`(begin #,e2 (frm e3s #,e1s (id ... . #,def-ids))))
		   ((begin-for-syntax . _)
		    #`(begin #,e2 (frm e3s #,e1s #,def-ids)))
		   ((begin b1 ...)
		    (syntax-track-origin 
		     (loop (append (syntax->list #'(b1 ...)) #'e3s) e1s def-ids)
		     e2
		     (car (syntax-e e2))))
		   ((define-values (id ...) . _)
		    (loop #'e3s (cons e2 e1s) (append (syntax->list #'(id ...)) def-ids)))
		   ((define-contract id sig)
		    (loop #'e3s (cons e2 e1s) def-ids))
		   ((define-record-procedures id cns prd (spec ...))
		    (loop #'e3s (cons e2 e1s) def-ids))
		   ((define-record-procedures-2 id cns prd (spec ...))
		    (loop #'e3s (cons e2 e1s) def-ids))
		   ((define-record-procedures-parametric id cns prd (spec ...))
		    (loop #'e3s (cons e2 e1s) def-ids))
		   ((define-record-procedures-parametric-2 id cns prd (spec ...))
		    (loop #'e3s (cons e2 e1s) def-ids))
		   ((: stuff ...)
		    (loop #'e3s (cons e2 e1s) def-ids))
		   (_
		    (loop #'e3s (cons #`(print-results #,e2) e1s) def-ids)))))))))))))
