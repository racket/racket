;; `package' and `open' correspond to Chez's `module' and `import' ---
;; without making `import' a part of the primitive expander mechanism,
;; which would require special handling for anything that uses
;; `local-expand'.
;;
;; The main idea is to hide package definitions by "introducing" the
;; identifier (i.e., applying a fresh mark for each definition).
;;
;; Beyond the main strategy, there are two major problems:
;;
;;  1. Making `package' declarations available to immediately
;;     following `open' declarations in an internal-definition
;;     context: (let () (package p ...) (open p) ...)
;;
;;     The problem is that `open' needs to inspect the package
;;     to decide what variables it binds, but the package
;;     descriptor isn't executed until the defn context has
;;     dertemined the full set of names to be defined.
;;
;;     We work around this problem by keeping our own table
;;     of "recently" processed `package' declarations. The
;;     `syntax-local-context' function lets us key this to
;;     specific internal-definition contexts.
;;
;;  2. Implementing the binding effect of an `open', which needs
;;     to expose the bindings hidden by a `package', but also
;;     needs to override shadowing.
;;
;;     The `syntax-local-get-shadower' MzScheme function provides
;;     the key ingredient for this part, but it doesn't quite work
;;     when `open' appears within `package'. In that case, we
;;     need to first take into account the package's introductions
;;     that hide definitions.

(module package mzscheme
  (require (lib "etc.ss")
	   (lib "stxparam.ss"))
  (require-for-syntax "private/package-helper.ss"
                      (lib "kerncase.ss" "syntax")
                      (lib "stx.ss" "syntax")
		      (lib "boundmap.ss" "syntax")
		      (lib "context.ss" "syntax")
		      (lib "define.ss" "syntax")
                      (lib "list.ss")
		      (lib "stxparam.ss"))

  (provide package package*
	   open define-dot
	   open* define*-dot
	   dot 
	   define*-syntax define*
	   define*-syntaxes define*-values
	   open/derived open*/derived package/derived
	   define-dot/derived define*-dot/derived
	   rename-potential-package rename*-potential-package)

  ;; Used to communicate to `open'
  ;; when an expression is within the body of a `package' declaration.
  ;; This matters for choosing the right shadower of an id.
  ;; The value of current-pack is a list of (cons id num),
  ;;  where num is the size of the applicable tail of the rename list
  ;;  for the package named id.
  (define-syntax-parameter current-package null)

  ;; The *ed define forms are the same as the usual
  ;; forms, except inside a package, where the
  ;; *ed names are specially detected.
  (define-syntax-set (define*-syntaxes
		       define*-values
		       define*-syntax
		       define*)
    (define (check-formals s)
      (let loop ([s s])
	(cond
	 [(stx-null? s) #t]
	 [(identifier? s) #t]
	 [(and (stx-pair? s)
	       (identifier? (stx-car s)))
	  (loop (stx-cdr s))]
	 [else #f])))

    (define (multi stx def)
      (syntax-case stx ()
	((_ (id ...) body) 
	 (andmap identifier? (syntax->list #'(id ...)))
	 (quasisyntax/loc stx (#,def (id ...) body)))))

    (define (define*-syntaxes/proc stx)
      (multi stx #'define-syntaxes))
    
    (define (define*-values/proc stx)
      (multi stx #'define-values))
  
    (define (single stx def-vals)
      (let-values ([(id rhs) (normalize-definition stx #'lambda)])
	(quasisyntax/loc stx (#,def-vals (#,id) #,rhs))))

    (define (define*-syntax/proc stx)
      (single stx #'define*-syntaxes))
    
    (define (define*/proc stx)
      (single stx #'define*-values)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; The main `package' implementation (actually, package/derived)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax-set (package/derived)

    ;; Adds the *ed "primitive" definition forms to the
    ;;  kernel-form list:
    (define kernel-form-identifier-list+defines
      (append (list #'define*-values #'define*-syntaxes)
	      (kernel-form-identifier-list #'here)))

    ;; Ensures that a single package element is a definition:
    (define (fix-expr e)
      (kernel-syntax-case e #f
        ((define-values x y) e)
        ((define-syntaxes x y) e)
        ((d x y) (and (identifier? #'d)
		      (or (module-identifier=? (quote-syntax define*-values) #'d)
			  (module-identifier=? (quote-syntax define*-syntaxes) #'d)))
         e)
        (x #`(define-values () (begin x (values))))))
    
    ;; Partially expands all body expressions, and wraps expressions
    ;; in empty `define-values'; the result is a list of definitions
    (define (get-defs expand-context defs exports)
      (let ([stop-list (append kernel-form-identifier-list+defines
			       exports)])
	(map fix-expr
	     (apply append
		    (map (letrec ([ex
				   (lambda (d)
				     (let ([_e (local-expand 
						d
						expand-context
						stop-list)])
				       (syntax-case _e (begin)
					 [(begin e ...)
					  (apply
					   append
					   (map (lambda (s)
						  (ex (syntax-track-origin s _e #'begin)))
						(syntax->list #'(e ...))))]
					 [else (list _e)])))])
			   ex)
			 defs)))))
    
    ;; Extracts all defined names, and also checks for duplicates
    ;; in the * forms.
    (define (extract-ids defs stx)
      (let loop ([defs defs][normal-defs null][let*-defs null])
	(if (null? defs)
	    (values normal-defs let*-defs)
	    (syntax-case (car defs) ()
	      [(dv (id ...) expr)
	       (and (identifier? #'dv)
		    (or (module-identifier=? #'dv #'define-values)
			(module-identifier=? #'dv #'define-syntaxes))
		    (andmap identifier? (syntax->list #'(id ...))))
	       (loop (cdr defs)
		     (append normal-defs (syntax->list #'(id ...)))
		     let*-defs)]
	      [(dv . _)
	       (and (identifier? #'dv)
		    (or (module-identifier=? #'dv #'define-values)
			(module-identifier=? #'dv #'define-syntaxes)))
	       (raise-syntax-error #f "bad syntax" (car defs))]
	      [(dv (id ...) expr)
	       (and (identifier? #'dv)
		    (or (module-identifier=? #'dv #'define*-values)
			(module-identifier=? #'dv #'define*-syntaxes))
		    (andmap identifier? (syntax->list #'(id ...))))
	       ;; Check that the identifiers in a single set are distinct
	       (let ([ids (syntax->list #'(id ...))])
		 (let ([dup (check-duplicate-identifier ids)])
		   (when dup
		     (raise-syntax-error
		      #f
		      "identifier defined multiple times in a single set"
		      stx
		      dup)))
		 (loop (cdr defs)
		       normal-defs
		       (append let*-defs ids)))]
	      [(dv . _)
	       (and (identifier? #'dv)
		    (or (module-identifier=? #'dv #'define*-values)
			(module-identifier=? #'dv #'define*-syntaxes)))
	       (raise-syntax-error #f "illegal definition form" (car defs))]))))

    ;; Extracts one set of starred names:
    (define (get/let*-ids def)
      (syntax-case def ()
        ((d vars body) (or (module-identifier=? (quote-syntax define*-values) #'d)
                           (module-identifier=? (quote-syntax define*-syntaxes) #'d))
         (syntax->list #'vars))
        (_ null)))
    
    ;; Combines parts of a transformed definition in a package:
    (define (rebuild-def orig package-name rename-length kw ids body compile-time?)
      (datum->syntax-object
       orig
       `(,kw ,ids ,(if compile-time?
		       body
		       #`(syntax-parameterize ([current-package
						(cons
						 (cons
						  (quote-syntax #,package-name)
						  #,rename-length)
						 (syntax-parameter-value
						  (quote-syntax current-package)))])
			   #,body)))
       orig
       orig))

    ;;  mark-ids : defn-stx 
    ;;             (list (cons id-stx (stx . -> . stx)))
    ;;             id-stx 
    ;;              ->  (list (cons id-stx (stx . -> . stx)))
    ;; Convert a definition from a package body, and add marks as
    ;; appropriate to map to hidden names within the package. Also
    ;; accumulate new hidden names from starred bindings.
    (define (mark-ids def introducers package-name expand-ctx)
      ;; Note: new-ids is null if this is a non-* definition
      (let ([new-ids (map (lambda (id) (cons id (make-syntax-introducer)))
                          (get/let*-ids def))]
	    [rename-length (length introducers)])
        (values
         (syntax-case def ()
           ((ds vars body) 
	    (module-identifier=? (quote-syntax define-syntaxes) #'ds)
	    (rebuild-def def package-name rename-length
			 #'ds 
			 (mark-to-localize #'vars (append new-ids introducers) #'protect) 
			 (mark-to-localize #'body (append new-ids introducers) #'protect)
			 #t))
           ((dv vars body)
	    (module-identifier=? (quote-syntax define-values) #'dv)
            (rebuild-def def package-name rename-length
			 #'dv 
			 (mark-to-localize #'vars (append new-ids introducers) #'protect) 
			 (mark-to-localize #'body (append new-ids introducers) #'protect)
			 #f))
           ((d vars body) 
	    (module-identifier=? (quote-syntax define*-values) #'d)
            (rebuild-def def package-name rename-length
			 #'define-values
			 (mark-to-localize #'vars (append new-ids introducers) #'protect)
			 (mark-to-localize #'body introducers #'protect)
			 #f))
           ((d vars body) 
	    (module-identifier=? (quote-syntax define*-syntaxes) #'d)
	    (rebuild-def def package-name rename-length
			 #'define-syntaxes
			 (mark-to-localize #'vars (append new-ids introducers) #'protect)
			 (mark-to-localize #'body introducers #'protect)
			 #t)))
	 new-ids)))
        
    ;; For top-level definitions, we need to "declare"
    ;; the defined variables before we might use them.
    ;; We declare the variable by compiling a dummy
    ;; define-values expression.
    (define (extract-declarations converted-defs)
      (let loop ([converted-defs converted-defs]
		 [pre-accum null])
	(if (null? converted-defs)
	    (values (reverse pre-accum))
	    (syntax-case (car converted-defs) (define-values)
	      [(define-values (id ...) body)
	       (loop (cdr converted-defs)
		     (list* #'(define-syntaxes (id ...) (values))
			    pre-accum))]
	      [_ (loop (cdr converted-defs)
		       pre-accum)]))))
    
    ;; The main package/derived transformer:
    (define (package/derived/proc derived-stx)
      (syntax-case derived-stx ()
        ((_ orig-stx name provides body ...)
         (let ([stx #'orig-stx])
	   ;; --- Error checking
	   (check-defn-context stx)
           (unless (identifier? #'name)
             (raise-syntax-error #f "structure name must be an identifier" stx #'name))
	   (unless (or (and (identifier? #'provides)
			    (module-identifier=? (quote-syntax all-defined) #'provides))
		       (and (stx-list? #'provides)
			    (andmap identifier? (stx->list #'provides))))
	     (if (eq? 'all-defined (syntax-e #'provides))
		 (raise-syntax-error
		  #f
		  "`all-defined' keyword has a binding, so it is disallowed as an export"
		  stx
		  #'provides)
		 (raise-syntax-error
		  #f
		  "exports must have the form `all-defined' or `(identifier ...)'"
		  stx
		  #'provides)))
	   (let ([specific-exports (if (identifier? #'provides)
				       #f
				       (syntax->list #'provides))])
	     (when specific-exports
	       (let ([dup (check-duplicate-identifier specific-exports)])
		 (when dup
		   (raise-syntax-error
		    #f
		    "identifier exported multiple times"
		    stx
		    dup))))
	     ;; --- Parse package body
	     (let*-values ([(expand-context) (build-expand-context (gensym 'package-define))]
			   [(defs) (get-defs expand-context
					     (syntax->list #'(body ...))
					     (or specific-exports
						 null))]
			   ;; normal-ids and let*-ids are in same order as in package:
			   [(normal-ids let*-ids) (extract-ids defs stx)]
			   [(bt) (make-bound-identifier-mapping)])
	       ;; --- More error checking (duplicate defns)
	       (for-each (lambda (id)
			   (when (bound-identifier-mapping-get bt id (lambda () #f))
			     (raise-syntax-error
			      #f
			      "identifier defined multiple times"
			      stx
			      id))
			   (bound-identifier-mapping-put! bt id #t))
			 normal-ids)
	       (for-each (lambda (id)
			   (when (bound-identifier-mapping-get bt id (lambda () #f))
			     (raise-syntax-error
			      #f
			      "identifier for * definition has a non-* definition"
			      stx
			      id)))
			 let*-ids)
	       ;; --- Convert package body, accumulating introducers
	       ;; The `defined-ids' variable is a (list (cons id-stx (stx . -> . stx)))
	       (let-values ([(converted-defs defined-ids)
			     (let loop ((defined-ids (map (lambda (id) (cons id (make-syntax-introducer)))
							  normal-ids))
					(defs defs)
					(accum null))
			       (cond
				((null? defs)
				 (values (reverse accum) defined-ids))
				(else
				 (let-values (((marked-def new-defined-ids)
					       (mark-ids (car defs) defined-ids #'name expand-context)))
				   (loop (append new-defined-ids defined-ids)
					 (cdr defs)
					 (cons marked-def accum))))))]
			    [(reverse-orig-ids) (reverse (append normal-ids let*-ids))])
		 ;; --- Create the list of exported identifiers
		 (let ([export-renames
			(remove-dups
			 (cond
			  [(not specific-exports)
			   (map (lambda (id)
				  (cons (car id)
					((cdr id) (car id))))
				defined-ids)]
			  [else
			   (map (lambda (provide)
				  (let ((introducer (stx-assoc provide defined-ids)))
				    (unless introducer
				      (raise-syntax-error
				       #f
				       "exported identifier not defined"
				       stx
				       provide))
				    (cons (car introducer)
					  ((cdr introducer) provide))))
				specific-exports)]))]
		       [all-renames (map (lambda (id)
					   (cons (car id)
						 ((cdr id) (car id))))
					 defined-ids)])
		   ;; --- Shuffle the package body to put syntax definitions first
		   (let ([pre-decls
			  (if (eq? 'top-level (syntax-local-context))
			      (extract-declarations converted-defs)
			      null)]
			 [converted-syntax-defs (filter (lambda (def)
							  (or (module-identifier=? (stx-car def) #'define-syntaxes)
							      (module-identifier=? (stx-car def) #'define*-syntaxes)))
							converted-defs)]
			 [converted-value-defs (filter (lambda (def)
							 (or (module-identifier=? (stx-car def) #'define-values)
							     (module-identifier=? (stx-car def) #'define*-values)))
						       converted-defs)])
		     ;; --- Register this package, in case an `open' appears before the
		     ;;     syntax definition is executed.
		     ;;       export-renames provides an (id-stx . id-stx) mapping for exported ids 
		     ;;       all-renames is (id-stx . id-stx) mapping for all ids (superset of export-renames)
		     (pre-register-package expand-context #'name export-renames all-renames defined-ids #'protect)
		     ;; --- Assemble the result
		     #`(begin
			 (define-syntaxes (name)
			   (make-str (list #,@(map (lambda (i)
						     ;; Use of `protect' keeps the id from being localized
						     ;; if this package is in another. That way, the
						     ;; source name in the mapping is always the original
						     ;; name.
						     #`(cons (protect #,(car i))
							     (quote-syntax #,(cdr i))))
						   export-renames))
				     (list #,@(map (lambda (i)
						     #`(cons (protect #,(car i))
							     (quote-syntax #,(cdr i))))
						   all-renames))))
			 #,@pre-decls
			 #,@converted-syntax-defs
			 #,@converted-value-defs))))))))))
    )

  (define-syntax (package* stx)
    (syntax-case stx ()
      [(package* name exports body ...)
       (with-syntax ([this-pkg (car (generate-temporaries '(this-pkg)))])
	 #`(begin
	     (package/derived #,stx this-pkg exports
			      body ...)
	     (rename*-potential-package name this-pkg)))]))
  
  (define-syntax (package stx)
    (syntax-case stx ()
      [(package* name exports body ...)
       #`(package/derived #,stx name exports
			  body ...)]))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; The main `open' implementation
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax-set (open/derived open*/derived open open* 
				   define-dot define*-dot define-dot/derived define*-dot/derived
				   rename-potential-package rename*-potential-package)
    (define (do-open stx orig-name
		     path
		     ex-name bind-name
		     def)
      (let* (;; If we're in an enclosing package's body, get it's rename environment, etc.
	     ;;   env is an (id-stx . id-stx) mapping - names exported by the package
	     ;;   rns is an (id-stx . id-stx) mapping - names defined in the package
	     ;;   subs is a table mapping defined id-stx to sub-package mappings
	     [cps (syntax-parameter-value #'current-package)]
	     [cp-env+rns+subs+ispre/s (map (lambda (cp) (open (car cp) (car cp) stx))
					   cps)]
	     ;; Reverse-map renaming due to being in a package body. In other words,
	     ;;  we find id "x", but it's been renamed because we're in an enclosing
	     ;;  package, and we're about to look in a table that maps original
	     ;;  names to something, so we need to reverse-map the name.
	     [cp-orig-name (lambda (id)
			     (let loop ([id id][cp-env+rns+subs+ispre/s cp-env+rns+subs+ispre/s][cps cps])
			       (if (null? cp-env+rns+subs+ispre/s)
				   id
				   (let ([in-pack-bind
					  (ormap (lambda (p)
						   (and (bound-identifier=? (cdr p) id)
							p))
						 (let ([l (cadr (car cp-env+rns+subs+ispre/s))])
						   (list-tail l (- (length l) (cdar cps)))))])
				     (loop (if in-pack-bind
					       (car in-pack-bind)
					       id)
					   (cdr cp-env+rns+subs+ispre/s)
					   (cdr cps))))))]
	     ;; Reverse-map renaming due to being in a package
	     ;;  body. For example, we have an "x" that we want to
	     ;;  shadow, but the correct shadower must use the new name
	     ;;  in the enclosing package.
	     [cp-current-name (lambda (id)
				(let loop ([id id][cp-env+rns+subs+ispre/s cp-env+rns+subs+ispre/s][cps cps])
				  (if (null? cp-env+rns+subs+ispre/s)
				      id
				      (let* ([l (cadr (car cp-env+rns+subs+ispre/s))]
					     [l (list-tail l (- (length l) (cdar cps)))])
					(let ([in-pack-bind (stx-assoc id l)])
					  (loop (if in-pack-bind
						    (cdr in-pack-bind)
						    id)
						(cdr cp-env+rns+subs+ispre/s)
						(cdr cps)))))))])
	;; Find the package. See above for a remainder of env+rns+subs+ispre.
	;; The `rename-chain' variable binds an (stx . -> stx).
	(let*-values ([(env+rns+subs+ispre rename-chain)
		       ;; Find the initial package. `open' reports an error if it can't find one
		       (let ([env+rns+subs+ispre (open (car path) orig-name stx)])
			 (walk-path (cdr path) env+rns+subs+ispre stx values cp-orig-name))])
	  (let* ([env (and env+rns+subs+ispre 
			   (let ([e (car env+rns+subs+ispre)])
			     (if ex-name
				 (let ([a (stx-assoc 
					   (let ([id (syntax-local-introduce ex-name)])
					     (cp-orig-name id))
					   (car env+rns+subs+ispre))])
				   (unless a
				     (raise-syntax-error
				      #f
				      "no such export from package"
				      stx
				      ex-name))
				   (list a))
				 e)))]
		 ;; Find the names that `open' is supposed to bind
		 [shadowers (if bind-name
				(list bind-name)
				(map (lambda (x)
				       (syntax-local-get-shadower
					(cp-current-name
					 (car x))))
				     env))])
	    ;; Set up the defined-name -> opened-name mapping
	    (with-syntax ([((pub . hid) ...)
			   (map (lambda (x shadower)
				  (cons (if bind-name
					    shadower ; which is bind-name
					    (syntax-local-introduce shadower))
					;; If the source module is defined in the same
					;; internal-definition context as this open, then we must
					;; introduce the use of the package export.
					;;
					;; If the source source module comes from elsewhere, we
					;; must not introduce it, in case the new and original
					;; name are the same (so the new binding might capture
					;; the reference to the original binding.
					;; 
					;; Note that if the names are the same and the
					;; context are the same, the choise doens't matter,
					;; because a dup-defn error will be reported.
					((if (cadddr env+rns+subs+ispre)
					     syntax-local-introduce 
					     values)
					 (cp-current-name (cdr x)))))
				env shadowers)]
			  [def-stxes def])
	      ;; In case another `open' follows this one in an
	      ;; internal-defn position, register renames for
	      ;; packages that we just made available:
	      (let* ([ctx (syntax-local-context)]
		     [subs (caddr env+rns+subs+ispre)])
		(when (pair? ctx)
		  (for-each (lambda (x shadower)
			      (re-pre-register-package subs ctx 
						       (if bind-name
							   (syntax-local-introduce shadower)
							   shadower)
						       (if subs
							   (car x)
							   (cdr x))))
			    env shadowers)))
	      ;; Open produces a syntax binding to map to the opened names:
	      (syntax/loc stx
		(def-stxes (pub ...)
		  (values (make-rename-transformer (quote-syntax hid)) ...))))))))

    (define (generic-open stx def)
      (check-defn-context stx)
      (syntax-case stx ()
	[(_ elem1 elem ...)
	 (do-open stx #f (syntax->list #'(elem1 elem ...))
		  #f #f
		  def)]))

    (define (generic-open/derived stx def)
      (syntax-case stx ()
	[(_ orig-stx name elem ...)
	 (do-open #'orig-stx #'name (syntax->list #'(elem ...))
		  #f #f
		  def)]))

    (define (open/proc stx)
      (generic-open stx #'define-syntaxes))
    (define (open*/proc stx)
      (generic-open stx #'define*-syntaxes))

    (define (open/derived/proc stx)
      (generic-open/derived stx #'define-syntaxes))
    (define (open*/derived/proc stx)
      (generic-open/derived stx #'define*-syntaxes))

    (define (do-define-dot stx def-stxes path bind-name)
      (unless (identifier? bind-name)
	(raise-syntax-error #f "not an identifier" stx bind-name))
      (let-values ([(path last) (split path)])
	(do-open stx #f
		 path
		 last bind-name
		 def-stxes)))

    (define (generic-define-dot stx def-stxes)
      (check-defn-context stx)
      (syntax-case stx ()
	((_ bind-name path1 path2 path3 ...)
	 (do-define-dot stx def-stxes (syntax->list #'(path1 path2 path3 ...)) #'bind-name))))

    (define (generic-define-dot/derived stx def-stxes)
      (check-defn-context stx)
      (syntax-case stx ()
	((_ orig-stx bind-name path1 path2 path3 ...)
	 (do-define-dot #'orig-stx def-stxes (syntax->list #'(path1 path2 path3 ...)) #'bind-name))))

    (define (define-dot/proc stx)
      (generic-define-dot stx #'define-syntaxes))

    (define (define*-dot/proc stx)
      (generic-define-dot stx #'define*-syntaxes))

    (define (define-dot/derived/proc stx)
      (generic-define-dot/derived stx #'define-syntaxes))

    (define (define*-dot/derived/proc stx)
      (generic-define-dot/derived stx #'define*-syntaxes))

    (define (do-rename stx def-stxes)
      (syntax-case stx ()
	[(_ new-name old-name)
	 (begin
	   (unless (identifier? #'new-name)
	     (raise-syntax-error #f "new name must be an identifier" stx #'new-name))
	   (unless (identifier? #'old-name)
	     (raise-syntax-error #f "old name must be an identifier" stx #'old-name))
	   ;; Re-register if in nested int-def context, and if old-name has
	   ;; a package mapping:
	   (let ([ctx (syntax-local-context)])
	     (when (list? ctx)
	       (re-pre-register-package #f (syntax-local-context)
					(syntax-local-introduce #'new-name)
					(syntax-local-introduce #'old-name))))
	   ;; Produce syntax-level renaming:
	   #`(#,def-stxes (new-name) (make-rename-transformer (quote-syntax old-name))))]))

    (define (rename-potential-package/proc stx)
      (do-rename stx #'define-syntaxes))
    (define (rename*-potential-package/proc stx)
      (do-rename stx #'define*-syntaxes)))
  
  (define-syntax (dot stx)
    (syntax-case stx ()
      ((_ path1 path2 path-rest ...)
       (let ([path (syntax->list #'(path1 path2 path-rest ...))])
	 (for-each (lambda (elem)
		     (unless (identifier? elem)
		       (raise-syntax-error
			#f
			"path element must be an identfier"
			stx
			elem)))
		   path)
	 (let*-values ([(path field) (split path)])
	   (quasisyntax/loc
	    stx
	    (let ()
	      (package this-pkg all-defined
		(open/derived #,stx #f #,@path))
	      (let-syntax ([#,field (lambda (stx)
				      (raise-syntax-error
				       #f
				       "no such exported identifier"
				       (quote-syntax #,stx)
				       stx))])
		(open/derived #f #f this-pkg)
		(let ()
		  #,field)))))))))

  )