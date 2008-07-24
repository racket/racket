(module kw mzscheme
  (require mzlib/class100)

  (provide (protect define-keywords
		    class100*/kw))

  ;; ---------------- Keyword propagation macros -------------------

  ;; Since we use class100 to construct the classes that users see,
  ;; keywords are not propagated by position automatically. So we use
  ;; the class100*/kw macro for every class exported to the user; it
  ;; explicitly includes all keywords supported through superclasses.
  ;; To avoid writing the same keyword sets over and over, we have
  ;; a define-keywords form.

  ;; Arguably, this is making a problem (using `class100' instead of
  ;; `class') worse as much as it solves the problem. Or maybe the
  ;; problem is trying to hard to make by-position and by-name
  ;; initialization work.

  (define-syntax (define-keywords stx)
    (syntax-case stx ()
      [(_ name kw ...)
       (with-syntax ([(kw2 ...)
		      (apply
		       append
		       (map (lambda (kw)
			      (if (identifier? kw)
				  (syntax-local-value kw)
				  (list kw)))
			    (syntax->list #'(kw ...))))])
	 #'(define-syntax name '(kw2 ...)))]))

  (define-syntax (class100*/kw stx)
    (syntax-case stx ()
      [(_ base (intf ...) ((base-init ...) keywords post-init ...) . rest)
       (let ([kws (syntax-local-value #'keywords)])
	 (with-syntax ([super-init (datum->syntax-object 
				    stx
				    'super-init
				    stx)]
		       [(new-keyword ...) (map car kws)]
		       [(new-init ...) (datum->syntax-object 
					stx
					kws)])
	   #'(let-syntax ([super-init
			   (lambda (sstx)
			     (syntax-case sstx ()
			       [(_ arg (... ...))
				(with-syntax ([super-instantiate
					       (datum->syntax-object 
						sstx
						'super-instantiate
						sstx)]
					      [(new-kw (... ...))
					       (map (lambda (x)
						      (datum->syntax-object 
						       sstx
						       x))
						    '(new-keyword ...))])
				  #'(super-instantiate (arg (... ...))
						       [new-kw new-kw] (... ...)))]))])
	       (class100*
		   base (intf ...) (base-init ... new-init ... post-init ...)
		   . rest))))])))
