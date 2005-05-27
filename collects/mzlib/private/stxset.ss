
(module stxset mzscheme

  (require-for-syntax (lib "kerncase.ss" "syntax")
		      (lib "context.ss" "syntax"))

  (provide finish-syntax-set)

  ;; Used in the expansion of `define-syntax-set' from "etc.ss"
  (define-syntax (finish-syntax-set stx)
    (syntax-case stx ()
      [(_ stx)
       (let ([stx (syntax stx)])
	 (syntax-case stx ()
	   [(_ (id ...) defn ...)
	    ;; The ids have already been checked --------------------
	    (let ([ids (syntax->list (syntax (id ...)))])
	      (let ([internal-ids (map (lambda (id)
					 (datum->syntax-object
					  id
					  (string->symbol (format "~a/proc" (syntax-e id)))
					  id))
				       ids)]
		    [expand-context (generate-expand-context)])
		;; Check defns (requires expand) ---------
		(let* ([defns (let loop ([defns (syntax->list (syntax (defn ...)))])
				(apply 
				 append
				 (map
				  (lambda (defn)
				    (let ([defn (local-expand
						 defn
						 expand-context
						 (kernel-form-identifier-list (quote-syntax here)))])
				      (syntax-case defn (define-values define-syntaxes begin)
					[(define-values (id ...) expr)
					 (andmap identifier? (syntax->list (syntax (id ...))))
					 (list defn)]
					[(define-values . _)
					 (raise-syntax-error
					  #f
					  "bad definition"
					  stx
					  defn)]
					[(define-syntaxes (id ...) expr)
					 (andmap identifier? (syntax->list (syntax (id ...))))
					 (list defn)]
					[(define-syntaxes . _)
					 (raise-syntax-error
					  #f
					  "bad definition"
					  stx
					  defn)]
					[(begin defn ...)
					 (loop (syntax->list (syntax (defn ...))))]
					[(begin . _)
					 (raise-syntax-error
					  #f
					  "bad `begin'"
					  stx
					  defn)]
					[else
					 (raise-syntax-error
					  #f
					  "not a definition"
					  stx
					  defn)])))
				  defns)))]
		       
		       [def-ids (apply append (map (lambda (defn)
						     (syntax-case defn ()
						       [(_ (id ...) expr)
							(syntax->list (syntax (id ...)))]))
						   defns))]
		       [val-ids (apply append (map (lambda (defn)
						     (syntax-case defn (define-values)
						       [(define-values (id ...) expr)
							(syntax->list (syntax (id ...)))]
						       [_else null]))
						   defns))])
		  (let ([dup (check-duplicate-identifier def-ids)])
		    (when dup
		      (raise-syntax-error
		       #f
		       "duplicate defined identifier"
		       stx
		       dup)))
		  ;; Check that declared are defined ---------
		  (for-each (lambda (id)
			      (unless (check-duplicate-identifier (cons id val-ids))
				(raise-syntax-error
				 #f
				 "expected identifier is not defined"
				 stx
				 id)))
			    internal-ids)
		  ;; Produce result ------------------------------
		  (with-syntax ([(defn ...) defns]
				[(internal-id ...) internal-ids])
		    (syntax/loc stx
		      (let ()
			defn ...
			(values internal-id ...)))))))]))])))
