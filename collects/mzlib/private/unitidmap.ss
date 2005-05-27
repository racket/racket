
(module unitidmap mzscheme

  ;; Help Desk binding info:
  (define (binding binder bound stx)
    stx
    ;; This 'bound-in-source is no longer needed
    #;
    (syntax-property
     stx
     'bound-in-source
     (cons binder (syntax-local-introduce bound))))

  (define (make-id-mapper unbox-stx the-binder)
    (let ([set!-stx (datum->syntax-object unbox-stx 'set! #f)])
      (make-set!-transformer
       (lambda (sstx)
	 (cond
	  [(identifier? sstx) 
	   (binding the-binder sstx
		    unbox-stx)]
	  [(module-identifier=? set!-stx (car (syntax-e sstx)))
	   (raise-syntax-error
	    'unit
	    "cannot set! imported or exported variables"
	    sstx)]
	  [else
	   (binding
	    the-binder (car (syntax-e sstx))
	    (datum->syntax-object
	     set!-stx
	     (cons unbox-stx (cdr (syntax-e sstx)))
	     sstx))])))))

  (provide make-id-mapper))

