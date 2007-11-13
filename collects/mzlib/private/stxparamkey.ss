
(module stxparamkey mzscheme

  (define-struct wrapped-renamer (renamer))
  
  (define-values (struct:syntax-parameter make-syntax-parameter syntax-parameter? syntax-parameter-ref syntax-parameter-set!)
    (make-struct-type 'syntax-parameter #f 2 0 #f null (current-inspector) 0))

  (define (syntax-parameter-target sp)
    (syntax-parameter-ref sp 1))

  (define (syntax-parameter-target-value target)
    (let ([v (syntax-local-value (syntax-local-get-shadower target)
				 (lambda ()
				   #f
				   (syntax-local-value 
				    target
				    (lambda () #f))))])
      (if (wrapped-renamer? v)
	  (wrapped-renamer-renamer v)
	  v)))

  (define (convert-renamer v)
    (if (rename-transformer? v)
	(make-wrapped-renamer v)
	v))

  (define (apply-transformer v stx set!-stx)
    (cond
     [(rename-transformer? v) 
      (with-syntax ([target (rename-transformer-target v)])
	(syntax-case stx ()
	  [(set! id _expr) 
	   (module-identifier=? #'set! set!-stx)
	   (syntax/loc stx (set! target expr))]
	  [(id . rest)
	   (let ([v (syntax (target . rest))])
	     (datum->syntax-object
	      stx
	      (syntax-e v)
	      stx))]
	  [_else
	   #'target]))]
     [(set!-transformer? v) ((set!-transformer-procedure v) stx)]
     [(and (procedure? v)
	   (procedure-arity-includes? v 1))
      (syntax-case stx ()
	[(set! id _) 
	 (module-identifier=? #'set! set!-stx)
	 (raise-syntax-error
	  #f
	  "cannot mutate syntax identifier"
	  stx
	  #'id)]
	[else (v stx)])]
     [else
      (raise-syntax-error
       #f
       "bad syntax"
       stx
       #f)]))


  (provide convert-renamer
	   apply-transformer
	   syntax-parameter?
	   make-syntax-parameter
	   syntax-parameter-target
	   syntax-parameter-target-value))
