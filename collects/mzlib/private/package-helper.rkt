(module package-helper mzscheme
  (require syntax/boundmap)

  (provide str str? str-renames str-all-renames make-str 
	   check-defn-context
	   pre-register-package
	   re-pre-register-package 
	   remove-dups stx-assoc mark-to-localize rebuild rebuild-cons
	   split open
	   protect
	   walk-path)

  ;; A compile-time struct for package info:
  (define-struct str (renames all-renames))
  ;; renames = renames for exports: (list (id-stx . id-stx) ...)
  ;; all-renames = all internal renames (needed to determine
  ;;               the appropriate shadowing variable when `open'
  ;;               appears in a `package' body)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; General utilities

  ;; Helper:
  (define (check-defn-context stx)
    (when (eq? 'expression (syntax-local-context))
      (raise-syntax-error
       #f
       "allowed only in definition contexts"
       stx)))
  
  ;; Removes dups from * defns
  (define (remove-dups l)
    (let ((ht (make-bound-identifier-mapping)))
      (let loop ((l l))
        (cond
          ((null? l) (bound-identifier-mapping-map ht (lambda (k v) v)))
          ((bound-identifier-mapping-get ht (caar l) (lambda () #f)) (loop (cdr l)))
          (else 
           (bound-identifier-mapping-put! ht (caar l) (car l))
	   (loop (cdr l)))))))
  
  (define (stx-assoc id renames)
    (cond
     ((null? renames) #f)
     ((bound-identifier=? id (caar renames)) (car renames))
     (else (stx-assoc id (cdr renames)))))
  
  (define insp (variable-reference->module-declaration-inspector
                (#%variable-reference)))

  (define (rebuild ctxt val)
    (if (syntax? ctxt)
	(datum->syntax-object ctxt val ctxt ctxt)
	val))
  
  (define (rebuild-cons car cdr stx)
    (rebuild stx (cons car cdr)))

  (define (split path)
    (let ((r (reverse path)))
      (values (reverse (cdr r)) (car r))))

  ;; The mark-to-localize function detects uses of `protect'
  ;; to prevent localization.
  (define-syntax protect
    (syntax-rules ()
      [(_ id) (quote-syntax id)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Side registration table of packages

  (define pre-registered (make-hash-table 'weak))
  ;; maps context keys to context-hash,
  ;; where a contetx hash maps id to (cons renames sub-context-hash-or-#f)

  (define (pre-register-package expand-ctx name renames all-renames introducers protect-stx)
    ;; expand-ctx is the context used for expanding the body;
    ;; if it's a list longer than 1, then the package itself was
    ;; expanded in an internal-def position. In that case,
    ;; we register the just-created package in the table. We
    ;; also remember any packages that were registered with
    ;; `(car expand-ctx)'.
    (when (> (length expand-ctx) 1)
      (let ([sub-ht (hash-table-get pre-registered (car expand-ctx) (lambda () #f))])
	(do-pre-register-package
	 (cadr expand-ctx)
	 (syntax-local-introduce name)
	 (list (map (lambda (i)
		      (cons (syntax-local-introduce (car i)) 
			    (syntax-local-introduce (cdr i))))
		    renames)
	       (map (lambda (i)
		      (cons (syntax-local-introduce (car i)) 
			    (syntax-local-introduce (cdr i))))
		    all-renames)
	       ;; Each package in the sub-ht table needs to be fixed
	       ;; up with the renamings introduced by the enclosing package.
	       (convert-subs sub-ht introducers protect-stx)
	       #t)))))

  ;; Just a hash-table wrapper
  (define (do-pre-register-package immediate-ctx name val)
    (let ([ht (hash-table-get pre-registered immediate-ctx
			      (lambda ()
				(let ([ht (make-bound-identifier-mapping)])
				  (hash-table-put! pre-registered immediate-ctx ht)
				  ht)))])
      (bound-identifier-mapping-put! 
       ht 
       name
       val)))

  ;; Gets info for a package that has been expanded but not
  ;; yet executed as syntax. Used only by get-renames.
  (define (get-pre-registered-package use-ctx name)
    (and (pair? use-ctx)
	 (ormap (lambda (ctx)
		  (let ([ht (hash-table-get pre-registered ctx (lambda () #f))])
		    (and ht
			 (bound-identifier-mapping-get ht 
						       name
						       (lambda () #f)))))
		use-ctx)))

  ;; When `open' exposes a package, we need to pre-register it.
  ;; Ditto for renaming a package.
  (define (re-pre-register-package subs expand-ctx name id)
    (let ([v (if subs
		 (bound-identifier-mapping-get subs id (lambda () #f))
		 (get-renames id (lambda (x) (lambda () #f)) #t))])
      (when v
	(do-pre-register-package
	 (car expand-ctx)
	 name
	 v))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Fixup

  ;; See pre-register-package. In each package's rename mappings,
  ;; the source is the original name, so it needs no conversion,
  ;; but the rename targets need to be the final names.
  ;; Recursively fix up nasted-package info.
  (define (convert-subs rn introducers protect-stx)
    (and rn
	 (let ([naya (make-bound-identifier-mapping)])
	   (bound-identifier-mapping-for-each
	    rn
	    (lambda (id v)
	      (bound-identifier-mapping-put!
	       naya
	       id
	       (list (map (lambda (i)
			    (cons (car i)
				  ;; The main fixup operation:
				  (intro-mark-to-localize (cdr i) introducers protect-stx)))
			  (car v))
		     (map (lambda (i)
			    (cons (car i)
				  ;; The main fixup operation:
				  (intro-mark-to-localize (cdr i) introducers protect-stx)))
			  (cadr v))
		     (convert-subs (caddr v) introducers protect-stx)
		     #t))))
	   naya)))

  ;; Mainly applies the introducers, but those introducers are
  ;;  for the package currently being expanded, and the saved
  ;;  syntax objects were stored in a table on the side from a
  ;;  previous expansion. So, we need to "introduce" the
  ;;  expressions before applying the renaming introducers, then
  ;;  un-introduce nack to neutral to store in the table.
  (define (intro-mark-to-localize def introducers protect-stx)
    (syntax-local-introduce
     (mark-to-localize (syntax-local-introduce def) introducers protect-stx)))

  ;; Traverses an S-expression, "introducing" identifiers
  ;; so that they refer to bindings that will be hidden 
  ;; by the package. Don't localize protected ids, though.
  (define (mark-to-localize def introducers protect-stx)
    (let ((contents 
	   (if (syntax? def)
	       (syntax-e def)
	       def)))
      (cond
       ((symbol? contents)
	(let ((introducer (stx-assoc def introducers)))
	  (if introducer ((cdr introducer) def) def)))
       ((pair? contents)
	(if (and (identifier? (car contents))
		 (module-transformer-identifier=? protect-stx (car contents)))
	    def
	    (rebuild-cons (mark-to-localize (car contents) introducers protect-stx)
			  (mark-to-localize (cdr contents) introducers protect-stx)
			  def)))
       ((vector? contents)
	(rebuild def (list->vector
		      (map (lambda (x) (mark-to-localize x introducers protect-stx))
			   (vector->list contents)))))
       (else def))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Open

  ;; Finds a package, either as a syntax definition or in the
  ;;  pre-registration table.
  ;; If the id was input to the current macro expander, it
  ;;  as been introduced (so we need to un-introduce it
  ;;  before using syntax-local-value).
  ;; The resulting env+rns+subs+ispre will have #f for subs when
  ;;  the package is found by `syntax-local-introduce'. In
  ;;  that case, sub-packages will be found in the
  ;;  environment, too.
  (define (get-renames id err try-pre?)
    (let ([env+rns+subs+ispre 
	   (or (and try-pre?
		    (get-pre-registered-package (syntax-local-context) id))
	       (let ([v (syntax-local-value (syntax-local-introduce id) (err id))])
		 (and (str? v)
		      (list (str-renames v) (str-all-renames v) #f #f))))])
      (or env+rns+subs+ispre
	  ((err id)))))
  
  ;; Wraps `get-renames' with suitable error handling.
  (define (open name orig-name stx)
    (unless (identifier? name)
      (raise-syntax-error #f "path component must be an identifier" stx name))
    (let ((err (lambda (name)
                 (lambda ()
                   (raise-syntax-error #f "unknown package" stx (if (identifier? orig-name)
								    orig-name 
								    name))))))
      (get-renames (syntax-local-introduce name) err #t)))

  ;; Given an initial package description (as env+rns+subs+ispre), find
  ;;  the innermost package indicated by `path'. The `stx'
  ;;  argument is for error reporting, the `rename' argument
  ;;  accumulates a renamer for environment lookups, and 
  ;;  `cp-rename' reverse-maps source ids for table lookups 
  ;;  (when we're in an enclosing package).
  ;; When env+rns+subs+ispre has a non-#f subs, then we'll always
  ;;  walk pre-registration info, and the renamer is
  ;;  not extended.
  (define (walk-path path env+rns+subs+ispre stx rename cp-rename)
    (let loop ([path path][env+rns+subs+ispre env+rns+subs+ispre][rename rename])
      (cond
       [(null? path) (values env+rns+subs+ispre rename)]
       [else (let* (;; Revser-map id, in case we're in an enclosing package:
		    [id (cp-rename (syntax-local-introduce (car path)))]
		    ;; If we have a sub-package table, it maps the 
		    ;;  original name, otherwise we need to search
		    ;;  based on the renamed package in an enclosing package.
		    [new-name (if (caddr env+rns+subs+ispre)
				  (cons id id)
				  (stx-assoc id (cadr env+rns+subs+ispre)))]
		    [v (and new-name
			    (if (caddr env+rns+subs+ispre)
				(bound-identifier-mapping-get (caddr env+rns+subs+ispre) 
							      (cdr new-name)
							      (lambda () #f))
				(get-renames (rename (cdr new-name))
					     (lambda (x) (lambda () #f))
					     #f)))])
	       (if v
		   (loop (cdr path) v (if (caddr env+rns+subs+ispre)
					  rename
					  (lambda (id)
					    (let ([a (stx-assoc id (cadr env+rns+subs+ispre))])
					      (rename (if a
							  (cdr a)
							  id))))))
		   (raise-syntax-error
		    #f
		    "no such exported subpackage"
		    stx
		    (car path))))])))
  
  )
