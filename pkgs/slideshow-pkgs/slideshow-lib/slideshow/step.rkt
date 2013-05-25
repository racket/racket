
(module step slideshow/slideshow
  (require mzlib/list
	   mzlib/etc)

  (provide with-steps with-steps~)
  
  (define-syntax (with-steps stx)
    (syntax-case stx ()
      [(_ (step-name ...) expr0 expr ...)
       #'(do-with-steps #f (step-name ...) expr0 expr ...)]))

  (define-syntax (with-steps~ stx)
    (syntax-case stx ()
      [(_ (step-name ...) expr0 expr ...)
       #'(do-with-steps #t (step-name ...) expr0 expr ...)]))

  (define-syntax (define-step stx)
    (syntax-case stx ()
      [(_ func id steps (arg ...) 
	  (((extra-arg ...) (def-arg ...)) ... 
	   ((all-extra-arg ...) ())) 
	  body)
       (syntax/loc stx
	 (begin
	   (define func 
	     (lambda (arg ... all-extra-arg ...)
	       body))
	   (define-syntax (id istx)
	     (syntax-case istx ()
	       [(_ arg ... extra-arg ...)
		(syntax/loc istx (_ arg ... extra-arg ... def-arg ...))]
	       ...
	       [(_ arg ... all-extra-arg ...)
		(begin
		  (unless (ormap (lambda (i)
				   (and (identifier? #'arg)
					(module-identifier=? i #'arg)))
				 (syntax->list (quote-syntax steps)))
		    (raise-syntax-error
		     #f
		     "unknown step name"
		     istx
		     #'arg))
		  ...
		  (syntax/loc istx (func (quote arg) ... all-extra-arg ...)))]))))]))

  (define-syntax (define-predicate/vproc stx)
    (syntax-case stx ()
      [(_ pred pred/p vproc proc steps (arg ...) body)
       #'(begin
	   (define-step pred/p pred steps (arg ...) ((() ())) body)
	   (define-step v proc steps (arg ...) (((f) (values))
						((f else-f) ()))
	     (if (pred/p arg ...) 
		 f
		 else-f))
	   (define-step v2 vproc steps (arg ...) ((() ()))
	     (if (pred/p arg ...) 
		 (let ([vproc (lambda (x) x)]) vproc)
		 (let ([vproc (lambda (x) (ghost x))]) vproc))))]))
       
  (define-syntax (do-with-steps stx)
    (syntax-case stx ()
      [(_ condensing (step-name ...) expr0 expr ...)
       (let ([capturing (lambda (s)
			  (datum->syntax-object #'expr0 s))])
	 (with-syntax ([only? (capturing 'only?)]
		       [vonly (capturing 'vonly)]
		       [only (capturing 'only)]
		       [except? (capturing 'except?)]
		       [vexcept (capturing 'vexcept)]
		       [except (capturing 'except)]
		       [before? (capturing 'before?)]
		       [vbefore (capturing 'vbefore)]
		       [before (capturing 'before)]
		       [after? (capturing 'after?)]
		       [vafter (capturing 'vafter)]
		       [after (capturing 'after)]
		       [between? (capturing 'between?)]
		       [vbetween (capturing 'vbetween)]
		       [between (capturing 'between)]
		       [between-excl? (capturing 'between-excl?)]
		       [vbetween-excl (capturing 'vbetween-excl)]
		       [between-excl (capturing 'between-excl)])
	   #'(let ([steps '(step-name ...)])
	       (map (lambda (step)
		      (define-predicate/vproc only? only?/p vonly only (step-name ...)
			(p)
			(eq? step p))
		      (define-predicate/vproc except? except?/p vexcept except (step-name ...)
			(p)
			(not (eq? step p)))
		      (define-predicate/vproc after? after?/p vafter after (step-name ...)
			(p)
			(memq step (or (memq p steps) null)))
		      (define-predicate/vproc before? vbefore?/p vbefore before (step-name ...)
			(p)
			(not (after?/p p)))
		      (define-predicate/vproc between? between?/p vbetween between (step-name ...)
			(p1 p2)
			(and (after?/p p1) (or (eq? step p2) (not (after?/p p2)))))
		      (define-predicate/vproc between-excl? between-excl?/p vbetween-excl between-excl (step-name ...)
			(p1 p2)
			(and (after?/p p1) (not (after?/p p2))))
		      (let () expr0 expr ...))
		    (if (and condensing condense?)
                        (begin
                          (skip-slides (sub1 (length steps)))
                          (last-pair steps))
			(if condense?
                            (let ([l (filter (lambda (id)
                                               (not (regexp-match #rx"~$" (symbol->string id))))
                                             steps)])
                              (skip-slides (- (length steps) (length l)))
                              l)
			    steps))))))])))
