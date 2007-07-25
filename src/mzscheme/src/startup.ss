;; This file is converted to [c]startup.inc and evaluated by
;; MzScheme's scheme_basic_env().

;; It implements, in a non-bootstrapping way, all of the MzScheme
;; syntax and "primitives" that are not implemented in the kernel.

;; Replace the content of this file to get a different set of initial
;; module definitions, initial imports, and initial variable bindings.

;; When using makefiles, `make startup' in [the build directory for]
;; plt/src/mzscheme creates plt/src/mzscheme/src/cstartup.inc. Note
;; that `make startup' requires a working MzScheme executable; see
;; schminc.h for information about avoiding cstartup.inc, and using
;; startup.inc (requires perl), instead. In fact, the recommend
;; build strategy for cstartup.inc is
;;   * Run configure in <builddir> with --enable-perl
;;   * Set USE_COMPILED_STARTUP in schminc.h to 0
;;   * Modify startup.ss to taste
;;   * Run make in <builddir>/mzscheme
;;   * Run make startup in <builddir>/mzscheme
;;   * Set USE_COMPILED_STARTUP in schminc.h to 1
;;   * Run make in <builddir>/mzscheme

;; Do not use block comments (with #| and |#) in this file. The
;; pre-processing script to build startup.inc can't handle them.

;; module.c contains a hack so that it assumes all modules defined
;; during start-up are purely functional (and can therefore be
;; evaluated lazily). So turn that off if necessary.

;;----------------------------------------------------------------------
;; basic syntax utilities

(module #%stx #%kernel

  ;; These utilities facilitate operations on syntax objects.
  ;; A syntax object that represents a parenthesized sequence
  ;; can contain a mixture of cons cells and syntax objects,
  ;; hence the need for `stx-null?', `stx-car', etc.

  ;; a syntax identifier?
  (define-values (identifier?)
    (lambda (p)
      (if (syntax? p)
	  (symbol? (syntax-e p))
	  #f)))

  ;; a syntax null?
  (define-values (stx-null?)
    (lambda (p)
      (if (null? p)
	  #t
	  (if (syntax? p) 
	      (null? (syntax-e p))
	      #f))))

  ;; null if a syntax null?, else #f
  (define-values (stx-null/#f)
    (lambda (p)
      (if (null? p)
	  null
	  (if (syntax? p) 
	      (if (null? (syntax-e p))
		  null
		  #f)
	      #f))))

  ;; a syntax pair?
  (define-values (stx-pair?)
    (lambda (p)
      (if (pair? p)
	  #t
	  (if (syntax? p)
	      (pair? (syntax-e p))
	      #f))))

  ;; a syntax list?
  (define-values (stx-list?)
    (lambda (p)
      (if (list? p)
	  #t
	  (if (syntax? p) 
	      (if (list? (syntax-e p))
		  #t
		  (letrec-values ([(loop)
                                   (lambda (l)
                                     (if (pair? l)
                                         (loop (cdr l))
                                         (stx-list? l)))])
                    (loop (syntax-e p))))
	      (if (pair? p)
		  (stx-list? (cdr p))
		  #f)))))

  ;; car of a syntax pair
  (define-values (stx-car)
    (lambda (p)
      (if (pair? p)
	  (car p)
	  (car (syntax-e p)))))

  ;; cdr of a syntax pair
  (define-values (stx-cdr)
    (lambda (p)
      (if (pair? p)
	  (cdr p)
	  (cdr (syntax-e p)))))

  ;; Flattens a syntax list into a list
  (define-values (stx->list)
    (lambda (e)
      (if (syntax? e)
	  (syntax->list e)
	  (let-values ([(flat-end)
                        (letrec-values ([(loop)
                                         (lambda (l)
                                           (if (null? l) 
                                               #f
                                               (if (pair? l)
                                                   (loop (cdr l))
                                                   (if (syntax? l) 
                                                       (syntax->list l)
                                                       #f))))])
                          (loop e))])
	    (if flat-end
		;; flatten
		(letrec-values ([(loop)
                                 (lambda (l)
                                   (if (null? l) 
                                       null
                                       (if (pair? l) 
                                           (cons (car l) (loop (cdr l)))
                                           (if (syntax? l) 
                                               flat-end))))])
                  (loop e))
		e)))))

  ;; a syntax vector?
  (define-values (stx-vector?)
    (lambda (p len)
      (if (syntax? p) 
	  (if (vector? (syntax-e p))
	      (if len
		  (= len (vector-length (syntax-e p)))
		  #t)
	      #f)
	  #f)))

  ;; syntax vector reference
  (define-values (stx-vector-ref)
    (lambda (p pos)
      (vector-ref (syntax-e p) pos)))

  ;; used in pattern-matching with an escape proc
  (define-values (stx-check/esc)
    (lambda (v esc)
      (if v
	  v
	  (esc #f))))

  ;; used in pattern-matching where #f on the cdr
  ;; is a failure
  (define-values (cons/#f)
    (lambda (i l)
      (if l
	  (cons i l)
	  #f)))

  ;; used in pattern-matching where either
  ;;  list can be a failure; if it's null, the first
  ;;  part might be an improper list
  (define-values (append/#f)
    (lambda (l1 l2)
      (if l1
	  (if l2
	      (if (null? l2)
		  l1
		  (append l1 l2))
	      #f)
	  #f)))

  ;; The rotate procedures are used to
  ;;  rotate a list of matches with multiple variables to
  ;;  get a list of multiple matches for single variables

  (define-values (stx-rotate)
    (lambda (l)
      (apply map list l)))

  (define-values (stx-rotate*)
    (lambda (l)
      (apply list* (apply map list l))))

  ;; The split procedure is used when matching ellipses
  ;;  fiollowed by a certain number of patterns
  (define-values (split-stx-list)
    (lambda (s n prop?)
      (let-values ([(pre post m)
		    (letrec-values ([(loop)
                                     (lambda (s)
                                       (if (stx-pair? s)
                                           (let-values ([(pre post m) (loop (stx-cdr s))])
                                             (if (< m n)
                                                 (values '() s (add1 m))
                                                 (values (cons (stx-car s) pre) post m)))
                                           (values '() s (if prop?
                                                             (if (stx-null? s) 
                                                                 0 
                                                                 -inf.0)
                                                             (if (stx-null? s)
                                                                 -inf.0
                                                                 1)))))])
                      (loop s))])
	(values pre post (= m n)))))

  (provide identifier? stx-null? stx-null/#f stx-pair? stx-list?
	   stx-car stx-cdr stx->list
	   stx-vector? stx-vector-ref
	   stx-check/esc cons/#f append/#f
	   stx-rotate stx-rotate*
	   split-stx-list))

;;----------------------------------------------------------------------
;; quasiquote

(module #%qq-and-or #%kernel
  (require-for-syntax #%stx #%kernel)
  
  (define-syntaxes (let let* letrec)
    (let-values ([(lambda-stx) (quote-syntax lambda-stx)]
                 [(letrec-values-stx) (quote-syntax letrec-values)])
      (let-values ([(go)
                    (lambda (stx named? star? target)
                      (define-values (stx-cadr) (lambda (x) (stx-car (stx-cdr x))))
                      (define-values (id-in-list?)
                        (lambda (id l)
                          (if (null? l)
                              #f
                              (if (bound-identifier=? id (car l)) 
                                  #t
                                  (id-in-list? id (cdr l))))))
                      (define-values (stx-2list?)
                        (lambda (x)
                          (if (stx-pair? x)
                              (if (stx-pair? (stx-cdr x))
                                  (stx-null? (stx-cdr (stx-cdr x)))
                                  #f)
                              #f)))
                      (if (if (not (stx-list? stx))
                              #t
                              (let-values ([(tail1) (stx-cdr stx)])
                                (if (stx-null? tail1)
                                    #t
                                    (if (stx-null? (stx-cdr tail1))
                                        #t
                                        (if named?
                                            (if (symbol? (syntax-e (stx-car tail1)))
                                                (stx-null? (stx-cdr (stx-cdr tail1)))
                                                #f)
                                            #f)))))
                          (raise-syntax-error #f "bad syntax" stx))
                      (let-values ([(name) (if named?
                                               (let-values ([(n) (stx-cadr stx)])
                                                 (if (symbol? (syntax-e n))
                                                     n
                                                     #f))
                                               #f)])
                        (let-values ([(bindings) (stx->list (stx-cadr (if name
                                                                          (stx-cdr stx)
                                                                          stx)))]
                                     [(body) (stx-cdr (stx-cdr (if name
                                                                   (stx-cdr stx)
                                                                   stx)))])
                          (if (not bindings)
                              (raise-syntax-error 
                               #f 
                               "bad syntax (not a sequence of identifier--expression bindings)" 
                               stx
                               (stx-cadr stx))
                              (let-values ([(new-bindings)
                                            (letrec-values ([(loop)
                                                             (lambda (l)
                                                               (if (null? l)
                                                                   null
                                                                   (let-values ([(binding) (car l)])
                                                                     (cons-immutable
                                                                      (if (stx-2list? binding)
                                                                          (if (symbol? (syntax-e (stx-car binding)))
                                                                              (if name
                                                                                  (cons (stx-car binding)
                                                                                        (stx-cadr binding))
                                                                                  (datum->syntax-object
                                                                                   lambda-stx
                                                                                   (cons-immutable (cons-immutable (stx-car binding)
                                                                                                                   null)
                                                                                                   (stx-cdr binding))
                                                                                   binding))
                                                                              (raise-syntax-error 
                                                                               #f 
                                                                               "bad syntax (not an identifier)" 
                                                                               stx
                                                                               (stx-car binding)))
                                                                          (raise-syntax-error 
                                                                           #f 
                                                                           "bad syntax (not an identifier and expression for a binding)" 
                                                                           stx
                                                                           binding))
                                                                      (loop (cdr l))))))])
                                              (loop bindings))])
                                (if star?
                                    (void)
                                    (if ((length new-bindings) . > . 5)
                                        (let-values ([(ht) (make-hash-table)])
                                          (letrec-values ([(check) (lambda (l)
                                                                     (if (null? l)
                                                                         (void)
                                                                         (let*-values ([(id) (if name
                                                                                                 (caar l)
                                                                                                 (stx-car (stx-car (car l))))]
                                                                                       [(idl) (hash-table-get ht (syntax-e id) null)])
                                                                           (if (id-in-list? id idl)
                                                                               (raise-syntax-error
                                                                                #f
                                                                                "duplicate identifier"
                                                                                stx
                                                                                id)
                                                                               (begin
                                                                                 (hash-table-put! ht (syntax-e id) (cons id idl))
                                                                                 (check (cdr l)))))))])
                                            (check new-bindings)))
                                        (letrec-values ([(check) (lambda (l accum)
                                                                   (if (null? l)
                                                                       (void)
                                                                       (let-values ([(id) (if name
                                                                                              (caar l)
                                                                                              (stx-car (stx-car (car l))))])
                                                                         (if (id-in-list? id accum)
                                                                             (raise-syntax-error
                                                                              #f
                                                                              "duplicate identifier"
                                                                              stx
                                                                              id)
                                                                             (check (cdr l) (cons id accum))))))])
                                          (check new-bindings null))))
                                (datum->syntax-object
                                 lambda-stx
                                 (if name
                                     (apply list-immutable
                                            (list-immutable 
                                             (quote-syntax letrec-values)
                                             (list-immutable
                                              (list-immutable
                                               (list-immutable name)
                                               (list*-immutable (quote-syntax lambda)
                                                                (apply list-immutable (map car new-bindings))
                                                                body)))
                                             name)
                                            (map cdr new-bindings))
                                     (list*-immutable target
                                                      new-bindings
                                                      body))
                                 stx))))))])
        (values
         (lambda (stx) (go stx #t #f (quote-syntax let-values)))
         (lambda (stx) (go stx #f #t (quote-syntax let*-values)))
         (lambda (stx) (go stx #f #f (quote-syntax letrec-values)))))))

  (define-values (qq-append)
    (lambda (a b)
      (if (list? a)
	  (append a b)
	  (raise-type-error 'unquote-splicing "proper list" a))))

  (define-syntaxes (quasiquote)
    (let-values ([(here) (quote-syntax here)] ; id with module bindings, but not lexical
                 [(unquote-stx) (quote-syntax unquote)]
                 [(unquote-splicing-stx) (quote-syntax unquote-splicing)])
      (lambda (in-form)
	(if (identifier? in-form)
	    (raise-syntax-error #f "bad syntax" in-form))
	(let-values
	    (((form) (if (stx-pair? (stx-cdr in-form))
			 (if (stx-null? (stx-cdr (stx-cdr in-form)))
			     (stx-car (stx-cdr in-form))
			     (raise-syntax-error #f "bad syntax" in-form))
			 (raise-syntax-error #f "bad syntax" in-form)))
	     ((normal)
	      (lambda (x old)
		(if (eq? x old)
		    (if (stx-null? x) 
			(quote-syntax ())
			(list (quote-syntax quote) x))
		    x)))
	     ((apply-cons)
	      (lambda (a d)
		(if (stx-null? d)
		    (list (quote-syntax list) a)
		    (if (if (pair? d)
			    (if (module-identifier=? (quote-syntax list) (car d))
				#t
				(module-identifier=? (quote-syntax list*) (car d)))
			    #f)
			(list* (car d) a (cdr d))
			(list (quote-syntax list*) a d))))))
	  (datum->syntax-object
	   here
	   (normal
	    (letrec-values
		(((qq)
		  (lambda (x level)
		    (let-values
			(((qq-list)
			  (lambda (x level)
			    (let-values
				(((old-first) (stx-car x)))
			      (let-values
				  (((old-second) (stx-cdr x)))
				(let-values
				    (((first) (qq old-first level)))
				  (let-values
				      (((second) (qq old-second level)))
				    (let-values
					()
				      (if (if (eq? first old-first)
					      (eq? second old-second)
					      #f)
					  x
					  (apply-cons
					   (normal first old-first)
					   (normal second old-second)))))))))))
		      (if (stx-pair? x)
			  (let-values
			      (((first) (stx-car x)))
			    (if (if (if (identifier? first)
					(module-identifier=? first unquote-stx)
					#f)
				    (stx-list? x)
				    #f)
				(let-values
				    (((rest) (stx-cdr x)))
				  (if (let-values
					  (((g35) (not (stx-pair? rest))))
					(if g35 g35 (not (stx-null? (stx-cdr rest)))))
				      (raise-syntax-error
				       'unquote
				       "expects exactly one expression"
				       in-form
				       x))
				  (if (zero? level)
				      (stx-car rest)
				      (qq-list x (sub1 level))))
				(if (if (if (identifier? first)
					    (module-identifier=? first (quote-syntax quasiquote))
					    #f)
					(stx-list? x)
					#f)
				    (qq-list x (add1 level))
				    (if (if (if (identifier? first)
						(module-identifier=? first unquote-splicing-stx)
						#f)
					    (stx-list? x)
					    #f)
					(raise-syntax-error
					 'unquote-splicing
					 "invalid context within quasiquote"
					 in-form
					 x)
					(if (if (stx-pair? first)
						(if (identifier? (stx-car first))
						    (if (module-identifier=? (stx-car first)
									     unquote-splicing-stx)
							(stx-list? first)
							#F)
						    #f)
						#f)
					    (let-values
						(((rest) (stx-cdr first)))
					      (if (let-values
						      (((g34) (not (stx-pair? rest))))
						    (if g34
							g34
							(not (stx-null? (stx-cdr rest)))))
						  (raise-syntax-error
						   'unquote
						   "expects exactly one expression"
						   in-form
						   x))
					      (let-values
						  (((uqsd) (stx-car rest))
						   ((old-l) (stx-cdr x))
						   ((l) (qq (stx-cdr x) level)))
						(if (zero? level)
						    (let-values
							(((l) (normal l old-l)))
						      (let-values
							  ()
							(list (quote-syntax qq-append) uqsd l)))
						    (let-values
							(((restx) (qq-list rest (sub1 level))))
						      (let-values
							  ()
							(if (if (eq? l old-l)
								(eq? restx rest)
								#f)
							    x
							    (apply-cons
							     (apply-cons
							      (quote-syntax (quote unquote-splicing))
							      (normal restx rest))
							     (normal l old-l))))))))
					    (qq-list x level))))))
			  (if (if (syntax? x) 
				  (vector? (syntax-e x))
				  #f)
			      (let-values
				  (((l) (vector->list (syntax-e x))))
				(let-values
				    (((l2) (qq l level)))
				  (let-values
				      ()
				    (if (eq? l l2)
					x
					(list (quote-syntax list->vector) l2)))))
			      (if (if (syntax? x) (box? (syntax-e x)) #f)
				  (let-values
				      (((v) (unbox (syntax-e x))))
				    (let-values
					(((qv) (qq v level)))
				      (let-values
					  ()
					(if (eq? v qv)
					    x
					    (list (quote-syntax box) qv)))))
				  x)))))))
	      (qq form 0))
	    form)
	   in-form)))))

  (define-syntaxes (and)
    (let-values ([(here) (quote-syntax here)])
      (lambda (x)
	(if (not (stx-list? x))
	    (raise-syntax-error #f "bad syntax" x))
	(let-values ([(e) (stx-cdr x)])
	  (if (stx-null? e)
	      (quote-syntax #t)
	      (if (if (stx-pair? e)
		      (stx-null? (stx-cdr e))
		      #t)
		  (stx-car e)
		  (datum->syntax-object
		   here
		   (list (quote-syntax if)
			 (stx-car e)
			 (cons (quote-syntax and)
			       (stx-cdr e))
			 (quote-syntax #f))
		   x)))))))

  (define-syntaxes (or)
    (let-values ([(here) (quote-syntax here)])
      (lambda (x)
	(if (identifier? x)
	    (raise-syntax-error #f "bad syntax" x))
	(let-values ([(e) (stx-cdr x)])
	  (if (stx-null? e) 
	      (quote-syntax #f)
	      (if (if (stx-pair? e)
		      (stx-null? (stx-cdr e))
		      #f)
		  (stx-car e)
		  (if (stx-list? e)
		      (let-values ([(tmp) 'or-part])
			(datum->syntax-object
			 here
			 (list (quote-syntax let) (list
						   (list
						    tmp
						    (stx-car e)))
			       (list (quote-syntax if)
				     tmp
				     tmp
				     (cons (quote-syntax or)
					   (stx-cdr e))))
			 x))
		      (raise-syntax-error 
		       #f
		       "bad syntax"
		       x))))))))

  (provide let let* letrec
           quasiquote and or))

;;----------------------------------------------------------------------
;; cond

(module #%cond #%kernel
  (require-for-syntax #%stx #%qq-and-or #%kernel)

  (define-syntaxes (cond)
    (let ([here (quote-syntax here)])
      (lambda (in-form)
	(if (identifier? in-form)
	    (raise-syntax-error #f "bad syntax" in-form))
	(datum->syntax-object
	 here
	 (let ([form (stx-cdr in-form)]
	       [serror
		(lambda (msg at)
		  (raise-syntax-error #f msg in-form at))])
	   (let loop ([tests form][first? #t])
	     (if (stx-null? tests)
		 (quote-syntax (void))
		 (if (not (stx-pair? tests))
		     (serror
		      "bad syntax (body must contain a list of pairs)"
		      tests)
		     (let ([line (stx-car tests)]
			   [rest (stx-cdr tests)])
		       (if (not (stx-pair? line))
			   (serror
			    "bad syntax (clause is not a test-value pair)"
			    line)
			   (let* ([test (stx-car line)]
                                  [value (stx-cdr line)]
                                  [else? (and (identifier? test)
                                              (module-identifier=? test (quote-syntax else)))])
			     (if (and else? (stx-pair? rest))
				 (serror "bad syntax (`else' clause must be last)" line))
			     (if (and (stx-pair? value)
				      (identifier? (stx-car value))
				      (module-identifier=? (stx-car value) (quote-syntax =>)))
				 (if (and (stx-pair? (stx-cdr value))
					  (stx-null? (stx-cdr (stx-cdr value))))
				     (let ([test (if else?
                                                     #t 
                                                     test)]
                                           [gen (gensym)])
				       `(,(quote-syntax let-values) ([(,gen) ,test])
					 (,(quote-syntax if) ,gen
					  (,(stx-car (stx-cdr value)) ,gen)
					  ,(loop rest #f))))
				     (serror
				      "bad syntax (bad clause form with =>)"
				      line))
				 (if else?
				     (if first?
					 ;; first => be careful not to introduce a splicable begin...
					 `(,(quote-syntax if) #t ,(cons (quote-syntax begin) value))
					 ;; we're in an `if' branch already...
					 (cons (quote-syntax begin) value))
				     (if (stx-null? value)
					 (let ([gen (gensym)])
					   `(,(quote-syntax let-values) ([(,gen) ,test])
					     (,(quote-syntax if) ,gen ,gen ,(loop rest #f))))
					 (list
					  (quote-syntax if) test
					  (cons (quote-syntax begin) value)
					  (loop rest #f))))))))))))
	 in-form))))

  (provide cond))

;;----------------------------------------------------------------------
;; record for static info produced by `define-struct'

(module #%struct-info #%kernel
  (require #%stx #%qq-and-or)

  (define-values (identifier/#f?)
    (lambda (x) (or (not x) (identifier? x))))

  ;; Check for a list containing all ids, except that last can be #f:
  (define-values (id/#f-list?)
    (lambda (id? x)
      (if (null? x)
	  #t
	  (if (pair? x)
	      (if (null? (cdr x))
		  (identifier/#f? (car x))
		  (and (id? (car x))
		       (id/#f-list? id? (cdr x))))
	      #f))))

  (define-values (struct-info?)
    (lambda (x)
      (and (list? x)
	   (= (length x) 6)
	   (identifier/#f? (car x))
	   (identifier/#f? (cadr x))
	   (identifier/#f? (caddr x))
	   (id/#f-list? identifier? (list-ref x 3))
	   (id/#f-list? identifier/#f? (list-ref x 4))
	   (or (identifier/#f? (list-ref x 5)) (eq? #t (list-ref x 5))))))

  (define-values (struct-info-type-id) car)
  (define-values (struct-info-constructor-id) cadr)
  (define-values (struct-info-predicate-id) caddr)
  (define-values (struct-info-accessor-ids) cadddr)
  (define-values (struct-info-mutator-ids) (lambda (x) (list-ref x 4)))

  (provide struct-info? 
	   struct-info-type-id
	   struct-info-constructor-id
	   struct-info-predicate-id
	   struct-info-accessor-ids
	   struct-info-mutator-ids))

;;----------------------------------------------------------------------
;; helper functions for `define-struct'

(module #%ds-helper #%kernel
  (require #%stx #%qq-and-or #%cond #%struct-info)
  
  (define-values (list->immutable-list)
    (lambda (l)
      (if (null? l) null (cons-immutable (car l) (list->immutable-list (cdr l))))))
  
  (define-values (get-stx-info)
    (lambda (orig-stx super-id defined-names gen-expr?)
      ;; Looks up super info, if needed, and builds compile-time info for the
      ;; new struct; called by all three forms, but does only half the work
      ;; if `defined-names' is #f.
      ;; If `expr?' is #t, then generate an expression to build the info,
      ;; otherwise build the info directly.
      (let ([cert-id (and gen-expr? (gensym))])
	(let ([qs (if gen-expr? (lambda (x) (and x `(,cert-id (quote-syntax ,x)))) values)]
	      [every-other (lambda (l)
			     (let loop ([l l][r null])
			       (cond
				[(null? l) r]
				[(null? (cdr l)) (cons (car l) r)]
				[else (loop (cddr l) (cons (car l) r))])))]
	      [super-info (and super-id 
			       (syntax-local-value super-id (lambda () #f)))])
	  (if super-id 
	      ;; Did we get valid super-info ?
	      (if (or (not (struct-info? super-info))
		      (not (struct-info-type-id super-info)))
		  (raise-syntax-error
		   #f
		   (if (struct-info? super-info)
		       "parent struct information does not include a type for subtyping"
		       (format "parent struct type not defined~a"
			       (if super-info
				   (format " (~a does not name struct type information)"
					   (syntax-e super-id))
				   "")))
		   orig-stx
		   super-id)))
	  (values
	   (if super-info
	       (struct-info-type-id super-info)
	       #f)
	   (if defined-names
	       (let-values ([(initial-gets initial-sets)
			     (if super-info
				 (values (map qs (struct-info-accessor-ids super-info))
					 (map qs (struct-info-mutator-ids super-info)))
				 (values null null))]
			    [(fields) (cdddr defined-names)]
			    [(wrap) (if gen-expr? (lambda (x) (cons 'list-immutable x)) values)]
			    [(total-wrap) (if gen-expr?
					      (lambda (x) `(let ([,cert-id (syntax-local-certifier #t)]) ,x))
					      values)])
		 (total-wrap
		  (wrap
		   (list-immutable (qs (car defined-names))
				   (qs (cadr defined-names))
				   (qs (caddr defined-names))
				   (wrap
				    (list->immutable-list
				     (append (map qs (every-other fields)) 
					     initial-gets)))
				   (wrap
				    (list->immutable-list
				     (append (map qs (if (null? fields) 
							 null 
							 (every-other (cdr fields)))) 
					     initial-sets)))
				   (if super-id
				       (qs super-id)
				       #t)))))
	       #f))))))

  (provide get-stx-info))

;;----------------------------------------------------------------------
;; -define, when, unless, let/ec, define-struct

(module #%define-et-al #%kernel
  (require-for-syntax #%kernel #%stx #%qq-and-or #%cond #%struct-info #%ds-helper)

  ;; No error checking here, because these macros merely help
  ;;  us write macros before the real define and define-syntax
  (define-syntaxes (-define -define-syntax)
    (let ([here (quote-syntax here)])
      (let ([mk-define
	     (lambda (base)
	       (lambda (code)
		 (let ([body (stx-cdr code)])
		   (let ([first (stx-car body)])
		     (cond
		      [(identifier? first)
		       (datum->syntax-object
			here
			`(,base (,first) ,@(stx->list (stx-cdr body)))
			code)]
		      [else
		       (let ([pbody (stx-cdr body)])
			 (datum->syntax-object
			  (quote-syntax here)
			  `(,base (,(stx-car first)) 
				  (lambda ,(stx-cdr first) ,@(stx->list pbody)))
			  code))])))))])
	(values (mk-define (quote-syntax define-values))
		(mk-define (quote-syntax define-syntaxes))))))

  (-define-syntax when
    (lambda (x)
      (let ([l (syntax->list x)])
	(if (and l
		 (> (length l) 2))
	    (datum->syntax-object
	     (quote-syntax here)
	     (list (quote-syntax if)
		   (stx-car (stx-cdr x))
		   (list*
		    (quote-syntax begin)
		    (stx-cdr (stx-cdr x))))
	     x)
	    (raise-syntax-error
	     #f
	     "bad syntax"
	     x)))))

  (-define-syntax unless
    (lambda (x)
      (let ([l (syntax->list x)])
	(if (and l
		 (> (length l) 2))
	    (datum->syntax-object
	     (quote-syntax here)
	     (list (quote-syntax if)
		   (cadr l)
		   (quote-syntax (void))
		   (list*
		    (quote-syntax begin)
		    (cddr l)))
	     x)
	    (raise-syntax-error
	     #f
	     "bad syntax"
	     x)))))

  (-define-syntax let/ec 
    (lambda (code)
      (let ([l (syntax->list code)])
	(if (and l
		 (> (length l) 2)
		 (identifier? (cadr l)))
	    (let ([var (cadr l)]
		  [exprs (stx-cdr (stx-cdr code))])
	      (datum->syntax-object
	       (quote-syntax here)
	       `(call/ec (lambda (,var) ,@(stx->list exprs)))
	       code))
	    (raise-syntax-error
	     #f
	     "bad syntax"
	     code)))))

  (define-syntaxes (define-struct)
    (let ([make-core
	   ;; generates the call to `make-struct-type'
	   (lambda (name inspector super-id/struct: field-names)
	     `(let-values ([(type maker pred access mutate)
			    (make-struct-type ',name
					      ,super-id/struct:
					      ,(length field-names)
					      0 #f null
					      ,inspector)])
		(values type maker pred
			,@(let loop ([field-names field-names][n 0])
			    (if (null? field-names)
				null
				(list* `(make-struct-field-accessor access ,n ',(car field-names))
				       `(make-struct-field-mutator mutate ,n ',(car field-names))
				       (loop (cdr field-names) (add1 n))))))))])
      ;; define-struct
      (lambda (stx)
	(if (identifier? stx)
	    (raise-syntax-error #f "bad syntax" stx))
	(let ([body (stx->list (stx-cdr stx))])
	  (let ([syntax-error
		 (lambda (s . detail)
		   (apply
		    raise-syntax-error
		    #f
		    s
		    stx
		    detail))]
		[build-struct-names
		 (lambda (name fields)
		   (let ([name (symbol->string (syntax-e name))]
			 [fields (map symbol->string (map syntax-e fields))]
			 [+ string-append])
		     (map string->symbol
			  (append
			   (list 
			    (+ "struct:" name)
			    (+ "make-" name)
			    (+ name "?"))
			   (apply
			    append
			    (map
			     (lambda (f) 
			       (list 
				(+ name "-" f)
				(+ "set-" name "-" f "!")))
			     fields))))))])
	    (or (pair? body)
		(syntax-error "empty declaration"))
	    (or (stx-list? body)
		(syntax-error "illegal use of `.'"))
	    (or (<= 2 (length body) 3)
		(syntax-error "wrong number of parts"))
	    (or (identifier? (car body))
		(and (stx-pair? (car body))
		     (identifier? (stx-car (car body)))
		     (stx-pair? (stx-cdr (car body)))
		     (identifier? (stx-car (stx-cdr (car body))))
		     (stx-null? (stx-cdr (stx-cdr (car body)))))
		(syntax-error "first part must be an identifier or pair of identifiers"))
	    (or (stx-list? (cadr body))
		(if (stx-pair? (cadr body))
		    (syntax-error "illegal use of `.' in field name sequence")
		    (syntax-error "field names must be a sequence")))
	    (for-each (lambda (x) 
			(or (identifier? x)
			    (syntax-error "field name not a identifier" x)))
		      (stx->list (cadr body)))
	    (if (memq (syntax-local-context) '(expression))
		(syntax-error "allowed only in definition contexts"))
	    (let ([name (if (identifier? (car body))
			    (car body)
			    (stx-car (car body)))]
		  [field-names (stx->list (cadr body))]
		  [inspector (if (null? (cddr body))
				 (quote-syntax (current-inspector))
				 (caddr body))]
		  [super-id (if (identifier? (car body))
				#f
				(stx-car (stx-cdr (car body))))])
	      (let ([defined-names (map 
				    (lambda (n) (datum->syntax-object name n name)) 
				    (build-struct-names name field-names))])
		(let-values ([(super-id/struct: stx-info) (get-stx-info stx super-id defined-names #t)])
		  (let ([result
			 (datum->syntax-object
			  (quote-syntax here)
			  `(begin
			     (define-values
			       ,defined-names
			       ,(let ([core (make-core name (and inspector 'inspector) super-id/struct: field-names)])
				  (if inspector
				      `(let-values ([(inspector) ,inspector])
					 (if (if inspector (not (inspector? inspector)) #f)
					     (raise-type-error 'define-struct "inspector or #f" inspector))
					 ,core)
				      core)))
			     (define-syntaxes (,name) ,stx-info))
			  stx)])
		    (if super-id
			(syntax-property result 
					 'disappeared-use 
					 (syntax-local-introduce super-id))
			result))))))))))

  (provide -define -define-syntax when unless let/ec define-struct))

;;----------------------------------------------------------------------
;; #%small-scheme: assembles all basic forms we have so far

(module #%small-scheme #%kernel
  (require #%stx #%qq-and-or #%cond #%define-et-al)

  (provide (all-from #%qq-and-or)
	   (all-from #%cond)
	   (all-from #%define-et-al)))

;;----------------------------------------------------------------------
;; pattern-matching utilities
;; based on Shriram's pattern matcher for Zodiac

(module #%sc #%kernel
  (require #%stx #%small-scheme)
  (require-for-template (only #%kernel set!))

  ;; Checks whether s is "..."
  (-define (...? s)
    (if (symbol? (syntax-e s))
	(module-identifier=? s (quote-syntax ...))
	#f))

  ;; memq on a list of identifiers, and
  ;;  nested identifiers
  (-define (stx-memq ssym l)
    (ormap (lambda (p)
	     (and (syntax? p)
		  (bound-identifier=? ssym p)))
	   l))
  
  ;; memq on a list of identifiers and
  ;;  nested identifiers, returns a position
  (-define (stx-memq-pos ssym l)
    (let loop ([p 0][l l])
      (cond
       [(null? l) #f]
       [(and (syntax? (car l))
	     (bound-identifier=? ssym (car l)))
	p]
       [else (loop (add1 p) (cdr l))])))

  ;; Like stx-memq-pos, but goes into nestings to
  ;;  find identifiers.
  (-define (stx-memq*-pos ssym l)
    (let loop ([p 0][l l])
      (cond
       [(null? l) #f]
       [(bound-identifier=? ssym 
			    (let loop ([i (car l)])
			      (if (syntax? i)
				  i
				  (loop (car i)))))
	p]
       [else (loop (add1 p) (cdr l))])))

  ;; For error reporting:
  (-define (pick-specificity e de)
    (if (eq? e de)
	(list e)
	(list e de)))

  ;;----------------------------------------------------------------------
  ;; Input matcher
  
  ;; Takes syntax pattern and a keyword list and produces a
  ;; matcher. A matcher is a function that takes a syntax input
  ;; and produces a pattern-variable environment or #f.
  ;;
  ;; If `just-vars?' is #t, produces the variables instead of a matcher.
  ;; Each variable is nested in the list corresponding to its ellipsis depth
  ;; in the pattern. We call this the "environment prototype". For reporting
  ;; left-to-right errors, we assume that the function will be called with
  ;; `just-vars?' as #t first, to catch errors.
  ;;
  ;; In the pattern-variable environment produced by a matcher,
  ;; a variable under a single ellipsis has a list of matches,
  ;; a variable under two ellipses has a list of list of matches, etc.
  ;; The top-level environment is a list* --- i.e., a list, except that the last
  ;; element is in the cdr of the cons cell for the next-to-last element.
  ;;
  ;; An environment does not contain any indication of how far a
  ;; variable is nested. Uses of the variable should be checked separately
  ;; using an environment prototype. Furthermore, the environment
  ;; does not contain the pattern variables as "keys", since the positions
  ;; can also be determined by the prototype.
  ;;
  (-define (make-match&env/extract-vars who top p k just-vars? phase-param?)
    ;;  The m&e function returns three values. If just-vars? is true,
    ;;  only the first result is used, and it is the variable list.
    ;;  Otherwise, the first result is the code assuming an input bound to `e'.
    ;;  The second result is #t if a variable was used, so that the code
    ;;  produces an environment rather than just a boolean.
    ;;  The last result is #t only when id-is-rest? was #t, and it indicates
    ;;  that the code refers to cap to get context for datum->syntax-object.
    (-define (m&e p local-top use-ellipses? last? id-is-rest?)
      (cond
       [(and use-ellipses? (ellipsis? p))
	(if (stx-null? (stx-cdr (stx-cdr p)))
	    ;; Simple case: ellipses at the end
	    (let* ([p-head (stx-car p)]
		   [nestings (get-ellipsis-nestings p-head k)])
	      (let-values ([(match-head mh-did-var? <false>) (m&e p-head p-head #t #f #f)])
		(if just-vars?
		    (values (map list nestings) #f #f)
		    (let ([nest-vars (flatten-nestings nestings (lambda (x) #t))])
		      (values
		       `(lambda (e)
			  (if (stx-list? e)
			      ,(let ([b (app-e match-head)])
				 (if (equal? b '(list e))
				     (if last?
					 '(stx->list e)
					 '(list (stx->list e)))
				     (if (null? nest-vars)
					 `(andmap (lambda (e) ,b) (stx->list e))
					 `(let/ec esc
					    (let ([l (map (lambda (e) (stx-check/esc ,b esc))
							  (stx->list e))])
					      (if (null? l)
						  (quote ,(let ([empties (map (lambda (v) '()) nest-vars)])
							    (if last?
								(apply list* empties)
								empties)))
						  (,(if last? 'stx-rotate* 'stx-rotate) l)))))))
			      #f))
		       mh-did-var?
		       #f)))))
	    ;; More stuff after ellipses. We need to make sure that
	    ;;  the extra stuff doesn't include any ellipses or a dot 
	    (let ([hd (list (stx-car p) (stx-car (stx-cdr p)))]
		  [rest (stx-cdr (stx-cdr p))])
	      (let-values ([(tail-cnt prop?)
			    (let loop ([rest rest][cnt 0])
			      (if (stx-null? rest)
				  (values cnt #t)
				  (if (stx-pair? rest)
				      (begin
					(when (...? (stx-car rest))
					  (raise-syntax-error 
					   (syntax-e who)
					   "misplaced ellipses in pattern (follows other ellipses)"
					   top
					   (stx-car rest)))
					(loop (stx-cdr rest) (add1 cnt)))
				      (values (add1 cnt) #f))))])
		;; Like cons case, but with a more elaborate assembly:
		(let*-values ([(-match-head -mh-did-var? <false>) (if just-vars?
								      (m&e hd hd use-ellipses? #f #f)
								      (values #f #f #f))]
			      [(match-tail mt-did-var? cap?) (m&e rest local-top use-ellipses? 
								  last? #t)]
			      [(match-head mh-did-var? <false>) (if just-vars?
								    (values -match-head -mh-did-var? #f)
								    (m&e hd hd use-ellipses? 
									 (and last? (not mt-did-var?))
									 #f))])
		  (if just-vars?
		      (values (append match-head match-tail) #f #f)
		      (values
		       `(lambda (e)
			  (let*-values ([(pre-items post-items ok?) 
					 (split-stx-list e ,tail-cnt ,prop?)])
			    (if ok?
				,(let ([s (let ([apph (app match-head 'pre-items)]
						[appt (app match-tail 'post-items)])
					    (if mh-did-var?
						(app-append apph appt)
						`(if ,apph ,appt #f)))])
				   (if cap?
				       (if id-is-rest?
					   `(let ([cap (if (syntax? e) e cap)]) ,s)
					   `(let ([cap e]) ,s))
				       s))
				#f)))
		       (or mh-did-var? mt-did-var?)
		       (and cap? id-is-rest?)))))))]
       [(stx-pair? p)
	(let ([hd (stx-car p)])
	  (if (and use-ellipses?
		   (...? hd))
	      (if (and (stx-pair? (stx-cdr p))
		       (stx-null? (stx-cdr (stx-cdr p))))
		  (let ([dp (stx-car (stx-cdr p))])
		    (m&e dp dp #f last? #f))
		  (raise-syntax-error 
		   (syntax-e who)
		   "misplaced ellipses in pattern"
		   top
		   hd))
	      ;; When just-vars?, do head first for good error ordering.
	      ;; Otherwise, do tail first to find out if it has variables.
	      (let*-values ([(-match-head -mh-did-var? <false>) (if just-vars?
								    (m&e hd hd use-ellipses? #f #f)
								    (values #f #f #f))]
			    [(match-tail mt-did-var? cap?) (m&e (stx-cdr p) local-top use-ellipses? 
								last? #t)]
			    [(match-head mh-did-var? <false>) (if just-vars?
								  (values -match-head -mh-did-var? #f)
								  (m&e hd hd use-ellipses? 
								       (and last? (not mt-did-var?))
								       #f))])
		(if just-vars?
		    (values (append match-head match-tail) #f #f)
		    (values
		     `(lambda (e)
			(if (stx-pair? e)
			    ,(let ([s (let ([apph (app match-head '(stx-car e))]
					    [appt (app match-tail '(stx-cdr e))])
					(if mh-did-var?
                                            (if mt-did-var?
                                                (app-append apph appt)
                                                `(let ([mh ,apph]) (and mh ,appt mh)))
					    `(if ,apph ,appt #f)))])
			       (if cap?
				   (if id-is-rest?
				       `(let ([cap (if (syntax? e) e cap)]) ,s)
				       `(let ([cap e]) ,s))
				   s))
			    #f))
		     (or mh-did-var? mt-did-var?)
		     (and cap? id-is-rest?))))))]
       [(stx-null? p)
	(if just-vars?
	    (values null #f #f)
	    (values 'stx-null/#f #f #f))]
       [(identifier? p)
	(if (stx-memq p k)
	    (if just-vars?
		(values null #f #f)
		(values
		 `(lambda (e)
		    (if (identifier? e)
			;; This module-identifier=? can be turned into
			;;  module-transformer-identifier=? by an
			;;  enclosing binding.
			(if (module-identifier=? e (quote-syntax ,p))
			    null
			    #f)
			#f))
		 #f
		 #f))
	    (if (and use-ellipses?
		     (...? p))
		(raise-syntax-error 
		 (syntax-e who)
		 "misplaced ellipses in pattern"
		 top
		 p)
		(if just-vars?
		    (values (list p) #f #f)
		    (values
		     (let ([wrap (if last?
				     (lambda (x) `(lambda (e) ,x))
				     (lambda (x) `(lambda (e) (list ,x))))])
		       (if id-is-rest?
			   (wrap '(datum->syntax-object cap e cap))
			   (wrap 'e)))
		     #t
		     id-is-rest?))))]
       [(stx-vector? p #f)
	(let ([l (vector->list (syntax-e p))])
	  ;; If no top-level ellipses, match one by one:
	  (if (and (not just-vars?)
		   (or (not use-ellipses?)
		       (andmap (lambda (x) (not (...? x))) l)))
	      ;; Match one-by-one:
	      ;; Do tail first to find out if it has variables.
	      (let ([len (vector-length (syntax-e p))])
		(let loop ([pos len][did-var? (not last?)][body null])
		  (if (zero? pos)
		      (values 
		       `(lambda (e)
			  (if (stx-vector? e ,len)
			      ,body
			      #f))
		       did-var?
		       #f)
		      (let-values ([(match-elem elem-did-var? <false>) 
				    (let ([e (vector-ref (syntax-e p) (sub1 pos))])
				      (m&e e e use-ellipses? (not did-var?) #f))])
			(loop (sub1 pos)
			      (or did-var? elem-did-var?)
			      (let ([app-elem (app match-elem `(stx-vector-ref e ,(sub1 pos)))])
				(if (null? body)
				    app-elem
				    (if elem-did-var?
					(app-append app-elem body)
					`(if ,app-elem ,body #f)))))))))
	      ;; Match as a list:
	      (let-values ([(match-content did-var? <false>) (m&e l p use-ellipses? last? #f)])
		(if just-vars?
		    (values match-content #f #f)
		    (values
		     `(lambda (e)
			(if (stx-vector? e #f)
			    ,(app match-content '(vector->list (syntax-e e)))
			    #f))
		     did-var?
		     #f)))))]
       [else
	(if just-vars?
	    (values null #f #f)
	    (values
	     `(lambda (e)
		(if ,(let ([test `(equal? ,(syntax-e p) (syntax-e e))])
		       (if id-is-rest? ; might get a syntax pair
			   `(and (syntax? e) ,test)
			   test))
		    null
		    #f))
	     #f
	     #f))]))
    (let-values ([(r did-var? <false>) (m&e p p #t #t #f)])
      (if just-vars?
	  ;; Look for duplicate uses of variable names:
	  (let ([ht (make-hash-table)])
	    (let loop ([r r])
	      (cond
	       [(syntax? r)
		(let ([l (hash-table-get ht (syntax-e r) null)])
		  (when (ormap (lambda (i) (bound-identifier=? i r)) l)
		    (raise-syntax-error 
		     (syntax-e who)
		     "variable used twice in pattern"
		     top
		     r))
		  (hash-table-put! ht (syntax-e r) (cons r l)))]
	       [(pair? r)
		(loop (car r))
		(loop (cdr r))]
	       [else (void)]))
	    r)
	  ;; A common trivial case is just return the expression
	  (if (equal? r '(lambda (e) e))
	      (if phase-param?
		  '(lambda (e module-identifier=?) e)
		  '(lambda (e) e))
	      `(lambda (e ,@(if phase-param?
				'(module-identifier=?) 
				null))
		 ,(app-e r))))))

  (-define (make-match&env who top p k phase-param?)
    (make-match&env/extract-vars who top p k #f phase-param?))
  
  (-define (get-match-vars who top p k)
    (make-match&env/extract-vars who top p k #t #f))

  ;; Create an S-expression that applies
  ;; rest to `e'. Optimize ((lambda (e) E) e) to E.
  (-define (app-e rest)
    (if (and (pair? rest)
	     (eq? (car rest) 'lambda)
	     (equal? (cadr rest) '(e)))
	(caddr rest)
	`(,rest e)))

  ;; Create an S-expression that applies
  ;; rest to e.
  (-define (app rest e)
    (if (and (pair? rest)
	     (eq? (car rest) 'lambda)
	     (equal? (cadr rest) '(e)))
	(let ([r (caddr rest)])
	  ;; special (common) case: body is `e' or `(list e)'
	  (cond
	   [(eq? r 'e)
	    e]
	   [(and (pair? r)
		 (eq? (car r) 'list)
		 (pair? (cdr r))
		 (eq? (cadr r) 'e)
		 (null? (cddr r)))
	    `(list ,e)]
	   [else
	    `(,rest ,e)]))
	`(,rest ,e)))

  ;; Create an S-expression that appends
  ;; e1 and e2. Optimize...
  (-define (app-append e1 e2)
    (if (and (pair? e1)
	     (eq? (car e1) 'list)
	     (pair? (cdr e1))
	     (null? (cddr e1)))
	`(cons/#f ,(cadr e1) ,e2)
	`(append/#f ,e1 ,e2)))

  ;; ----------------------------------------------------------------------
  ;; Output generator

  ;; Takes a syntax pattern, an environment prototype, and
  ;; a keyword symbol list, and produces an expander
  ;; that takes an environment and produces syntax.
  ;;
  ;; If the environment prototype is #f, it produces a list of
  ;; variables used in the pattern, instead. This is useful for
  ;; determining what kind of environment (and prototype) to construct
  ;; for the pattern.
  ;;
  ;; An environment for an expander is a list*; see the note above,
  ;; under "Input Matcher", for details.
  ;;
  (-define (make-pexpand p proto-r k dest)
    (-define top p)
    (-define (expander p proto-r local-top use-ellipses? use-tail-pos hash!)
      (cond
       [(and use-ellipses? (ellipsis? p))
	(let*-values ([(p-head) (stx-car p)]
		      [(el-count rest-p last-el)
		       (let loop ([p (stx-cdr (stx-cdr p))][el-count 0][last-el (stx-car (stx-cdr p))])
			 (if (and (stx-pair? p)
				  (...? (stx-car p)))
			     (loop (stx-cdr p) (add1 el-count) (stx-car p))
			     (values el-count p last-el)))]
		      [(p-head) (let loop ([el-count el-count])
				  (if (zero? el-count)
				      p-head
				      (datum->syntax-object
				       #f
				       (list (loop (sub1 el-count)) (quote-syntax ...)))))]
		      [(nestings) (and proto-r (get-ellipsis-nestings p-head k))])
	  (when (null? nestings)
	    (apply
	     raise-syntax-error 
	     'syntax
	     "no pattern variables before ellipses in template"
	     (pick-specificity
	      top
	      last-el)))
	  (let* ([proto-rr+deep?s (and proto-r
				       (map (lambda (nesting)
					      (ellipsis-sub-env nesting proto-r top local-top))
					    nestings))]
		 [proto-rr-deep (and proto-r
				     (let loop ([l proto-rr+deep?s])
				       (cond
					[(null? l) null]
					[(cdar l) (loop (cdr l))]
					[else (cons (caar l) (loop (cdr l)))])))]
		 [proto-rr-shallow (and proto-r
					(let loop ([l proto-rr+deep?s])
					  (cond
					   [(null? l) null]
					   [(cdar l) (cons (caar l) (loop (cdr l)))]
					   [else (loop (cdr l))])))]
		 [flat-nestings-deep (and proto-r (extract-vars proto-rr-deep))]
		 [flat-nestings-shallow (and proto-r (extract-vars proto-rr-shallow))]
		 [_ (unless (null? proto-rr-shallow)
		      (when (null? proto-rr-deep)
			(apply
			 raise-syntax-error 
			 'syntax
			 "too many ellipses in template"
			 (pick-specificity
			  top
			  last-el))))]
		 [rest (expander rest-p proto-r local-top #t use-tail-pos hash!)]
		 [ehead (expander p-head (and proto-r (append proto-rr-shallow proto-rr-deep)) p-head #t #f hash!)])
	    (if proto-r
		`(lambda (r)
		   ,(let ([pre (let ([deeps
				      (let ([valses
					     (map (lambda (var)
						    (apply-list-ref 'r (stx-memq*-pos var proto-r) use-tail-pos))
						  flat-nestings-deep)])
					(cond
					 [(and (= 1 (length valses))
					       (= 0 el-count)
					       (null? flat-nestings-shallow)
					       (equal? ehead '(lambda (r) (car r))))
					  ;; Common case: one item in list, no map needed:
					  (car valses)]
					 [(and (= 2 (length valses))
					       (= 0 el-count)
					       (null? flat-nestings-shallow)
					       (equal? ehead '(lambda (r) (list (car r) (cadr r)))))
					  ;; Another common case: a maintained pair
					  `(map 
					    (lambda (a b) (list a b))
					    ,@valses)]
					 [else
					  ;; General case:
					  (letrec ([wrap (lambda (expr el-count)
							   (if (zero? el-count)
							       expr
							       (wrap `(apply append ,expr)
								     (sub1 el-count))))])
					    (wrap
					     `(map 
					       (lambda vals (,ehead 
							     ,(if (null? flat-nestings-shallow)
								  'vals
								  '(append shallows vals))))
					       ,@valses)
					     el-count))]))])
				 (if (null? flat-nestings-shallow)
				     deeps
				     `(let ([shallows
					     (list ,@(map (lambda (var)
							    (apply-list-ref 'r (stx-memq*-pos var proto-r) use-tail-pos))
							  flat-nestings-shallow))])
					,deeps)))]
			  [post (apply-to-r rest)])
		      (if (eq? post 'null)
			  pre
			  `(append ,pre ,post))))
		;; variables were hashed
		(void))))]
       [(stx-pair? p)
	(let ([hd (stx-car p)])
	  (if (and use-ellipses?
		   (...? hd))
	      (if (and (stx-pair? (stx-cdr p))
		       (stx-null? (stx-cdr (stx-cdr p))))
		  (let ([dp (stx-car (stx-cdr p))])
		    (expander dp proto-r dp #f use-tail-pos hash!))
		  (raise-syntax-error 
		   'syntax
		   "misplaced ellipses in template"
		   top
		   hd))
	      (let ([ehd (expander hd proto-r hd use-ellipses? use-tail-pos hash!)]
		    [etl (expander (stx-cdr p) proto-r local-top use-ellipses? use-tail-pos hash!)])
		(if proto-r
		    `(lambda (r)
		       ,(apply-cons p (apply-to-r ehd) (apply-to-r etl) p))
		    ;; variables were hashed
		    (void)))))]
       [(stx-vector? p #f)
	(let ([e (expander (vector->list (syntax-e p)) proto-r p use-ellipses? use-tail-pos hash!)])
	  (if proto-r
	      `(lambda (r)
		 (list->vector (stx->list ,(apply-to-r e))))
	      ;; variables were hashed
	      (void)))]
       [(identifier? p)
	(if (stx-memq p k) 
	    (if proto-r 
		`(lambda (r) (quote-syntax ,p))
		(void))
	    (if proto-r
		(let ((x (stx-memq p proto-r)))
		  (if x 
		      `(lambda (r) ,(apply-list-ref 'r (stx-memq-pos p proto-r) use-tail-pos))
		      (begin
			(when (and use-ellipses?
				   (...? p))
			  (raise-syntax-error 
			   'syntax
			   "misplaced ellipses in template"
			   top
			   p))
			(check-not-pattern p proto-r)
			`(lambda (r) (quote-syntax ,p)))))
		(unless (and (...? p)
			     use-ellipses?)
		  (hash! p))))]
       [(null? p) 
	;; Not syntax, so avoid useless syntax info
	(if proto-r 
	    `(lambda (r) null)
	    (void))]
       [else (if proto-r 
		 `(lambda (r) (quote-syntax ,p))
		 (void))]))
    (let* ([ht (if proto-r
		   #f
		   (make-hash-table))]
	   [l (expander p proto-r p #t
			(and proto-r (sub1 (length proto-r)))
			(if proto-r
			    #f
			    (lambda (r)
			      (let ([l (hash-table-get ht (syntax-e r) null)])
                                (let ([pr (and (pair? l)
                                               (ormap (lambda (i) 
                                                        (and (bound-identifier=? (car i) r) i))
                                                      l))])
                                  (if pr
                                      (set-cdr! pr (cons r (cdr pr)))
                                      (hash-table-put! ht (syntax-e r) (cons (cons r (list r)) l))))))))])
      (if proto-r
	  `(lambda (r)
	     ,(let ([main (let ([build (apply-to-r l)])
			    (if (and (pair? build)
				     (eq? (car build) 'pattern-substitute))
				build
				(let ([small-dest ;; In case dest has significant structure...
				       (and dest (datum->syntax-object
						  dest
						  'dest
						  dest
						  dest))])
				  `(datum->syntax-object/shape (quote-syntax ,small-dest)
							       ,build))))])
		(if (multiple-ellipsis-vars? proto-r)
		    `(catch-ellipsis-error
		      (lambda () ,main)
		      (quote ,p)
		      ;; This is a trick to minimize the syntax structure we keep:
		      (quote-syntax ,(datum->syntax-object #f '... p)))
		    main)))
          (let ([l (apply append (hash-table-map ht (lambda (k v) v)))])
            (values
             ;; Get list of unique vars:
             (map car l)
             ;; All ids, including duplicates:
             (map cdr l))))))

  ;; apply-to-r creates an S-expression that applies
  ;; rest to `r', but it also optimizes ((lambda (r) E) r)
  ;; as simply E.
  (-define (apply-to-r rest)
    (if (and (pair? rest)
	     (eq? (car rest) 'lambda)
	     (equal? (cadr rest) '(r)))
	(caddr rest)
	`(,rest r)))

  ;; creates an S-expression that conses h and t,
  ;; with optimizations. If h and t are quoted
  ;; versions of the car and cdr of p, then return
  ;; a quoted as the "optimization" --- one that
  ;; is necessary to preserve the syntax wraps
  ;; associated with p.
  (-define (apply-cons stx h t p)
    (cond
     [(and (pair? h)
	   (eq? (car h) 'quote-syntax)
	   (eq? (cadr h) (stx-car p))
	   (or (eq? t 'null)
	       (and
		(pair? t)
		(eq? (car t) 'quote-syntax)
		(eq? (cadr t) (stx-cdr p)))))
      `(quote-syntax ,p)]
     [(and (pair? t)
	   (eq? (car t) 'pattern-substitute))
      ;; fold h into the existing pattern-substitute:
      (cond
       [(and (pair? h)
	     (eq? (car h) 'quote-syntax)
	     (eq? (cadr h) (stx-car p)))
	;; Just extend constant part:
	`(pattern-substitute
	  (quote-syntax ,(let ([v (cons (cadr h) (cadadr t))])
			   ;; We exploit the fact that we're
			   ;;  building an S-expression to
			   ;;  preserve the source's distinction
			   ;;  between (x y) and (x . (y)).
			   (if (syntax? stx)
			       (datum->syntax-object stx
						     v
						     stx
						     stx
                                                     stx)
			       v)))
	  . ,(cddr t))]
       [(and (pair? h)
	     (eq? 'pattern-substitute (car h)))
	;; Combine two pattern substitutions:
	`(pattern-substitute (quote-syntax ,(let ([v (cons (cadadr h) (cadadr t))])
					      (if (syntax? stx)
						  (datum->syntax-object stx
									v
									stx
									stx
                                                                        stx)
						  v)))
			     ,@(cddr h) ;; <-- WARNING: potential quadratic expansion
			     . ,(cddr t))]
       [else
	;; General case: add a substitution:
	(let* ([id (gensym)]
	       [expr (cons id (cadadr t))]
	       [expr (if (syntax? stx)
			 (datum->syntax-object stx
					       expr
					       stx
					       stx
                                               stx)
			 expr)])
	  `(pattern-substitute
	    (quote-syntax ,expr)
	    ,id ,h
	    . ,(cddr t)))])]
     [(eq? t 'null)
      (apply-cons stx h 
		  `(pattern-substitute (quote-syntax ()))
		  p)]
     [(and (pair? t)
	   (eq? (car t) 'quote-syntax)
	   (stx-smaller-than? (car t) 10))
      ;; Shift into `pattern-substitute' mode with an intitial constant.
      ;; (Only do this for small constants, so we don't traverse
      ;; big constants when looking for substitutions.)
      (apply-cons stx h 
		  `(pattern-substitute ,t)
		  p)]
     [else
      ;; Shift into `pattern-substitute' with an initial substitution:
      (apply-cons stx h
		  (let ([id (gensym)])
		    `(pattern-substitute (quote-syntax ,id)
					 ,id ,t))
		  p)]))

  (-define (stx-smaller-than? stx sz)
    (sz . > . (stx-size stx (add1 sz))))

  (-define (stx-size stx up-to)
    (cond
     [(up-to . < . 1) 0]
     [(syntax? stx) (stx-size (syntax-e stx) up-to)]
     [(pair? stx) (let ([s1 (stx-size (car stx) up-to)])
		    (+ s1 (stx-size (cdr stx) (- up-to s1))))]
     [(vector? stx) (stx-size (vector->list stx) up-to)]
     [(box? stx) (add1 (stx-size (unbox stx) (sub1 up-to)))]
     [else 1]))

  ;; Generates a list-ref expression; if use-tail-pos
  ;;  is not #f, then the argument list is really a list*
  ;;  (see the note under "Input Matcher") and in that case
  ;;  use-tail-pos is a number indicating the list-tail
  ;;  position of the last element
  (-define (apply-list-ref e p use-tail-pos)
    (cond
     [(and use-tail-pos (= p use-tail-pos))
      (cond
       [(eq? p 0) e]
       [(eq? p 1) `(cdr ,e)]
       [(eq? p 2) `(cddr ,e)]
       [(eq? p 3) `(cdddr ,e)]
       [(eq? p 4) `(cddddr ,e)]
       [else `(list-tail ,e ,p)])]
     [(eq? p 0) `(car ,e)]
     [(eq? p 1) `(cadr ,e)]
     [(eq? p 2) `(caddr ,e)]
     [(eq? p 3) `(cadddr ,e)]
     [else `(list-ref ,e ,p)]))

  ;; Returns a list that nests a pattern variable as deeply as it
  ;; is ellipsed. Escaping ellipses are detected.
  (-define get-ellipsis-nestings
    (lambda (p k)
      (let sub ([p p][use-ellipses? #t])
	(cond 
	 [(and use-ellipses? (ellipsis? p))
	  (let-values ([(rest nest)
			(let loop ([p (stx-cdr (stx-cdr p))][nest list])
			  (if (and (stx-pair? p)
				   (...? (stx-car p)))
			      (loop (stx-cdr p) (lambda (x) (list (nest x))))
			      (values p nest)))])
	    (let ([subs (sub (stx-car p) #t)])
	      (append (map nest subs)
		      (sub rest #t))))]
	 [(stx-pair? p) 
	  (let ([hd (stx-car p)])
	    (if (and use-ellipses?
		     (identifier? hd)
		     (...? hd)
		     (stx-pair? (stx-cdr p)))
		(sub (stx-car (stx-cdr p)) #f)
		(append! (sub (stx-car p) use-ellipses?) 
			 (sub (stx-cdr p) use-ellipses?))))]
	 [(identifier? p)
	  (if (stx-memq p k) 
	      '() 
	      (list p))]
	 [(stx-vector? p #f)
	  (sub (vector->list (syntax-e p)) use-ellipses?)]
	 [else '()]))))

  ;; Checks whether the given nesting matches a nesting in the
  ;; environment prototype, returning the prototype entry if it is
  ;; found, and signaling an error otherwise. If the prototype 
  ;; entry should be unwrapped by one, it is, and the resulting
  ;; prototype is paired with #f. Otherwise, the prototype is left
  ;; alone and paired with #t.
  (-define ellipsis-sub-env
    (lambda (nesting proto-r src detail-src)
      (let ([v (ormap (lambda (proto)
			(let ([start (if (pair? proto)
					 (car proto)
					 proto)])
			  (let loop ([c start] [n nesting] [unwrap? (pair? proto)])
			    (cond
			     [(and (pair? c) (pair? n))
			      (loop (car c) (car n) #t)]
			     [(pair? n)
			      (loop c (car n) #f)]
			     [(and (syntax? c) (syntax? n))
			      (if (bound-identifier=? c n)
				  (cons (if unwrap? start proto)
					(not unwrap?))
				  #f)]
			     [else #f]))))
		      proto-r)])
	(unless v
	  (apply
	   raise-syntax-error 
	   'syntax
	   "too few ellipses for pattern variable in template"
	   (pick-specificity
	    src
	    (let loop ([n nesting])
	      (if (syntax? n)
		  n
		  (loop (car n)))))))
	v)))

  (-define (extract-vars proto-r)
    (map (lambda (i)
	   (let loop ([i i])
	     (if (syntax? i)
		 i
		 (loop (car i)))))
	 proto-r))

  ;; Checks that a variable is not in the prototype
  ;; environment, and specifically not an ellipsed
  ;; variable.
  (-define (check-not-pattern ssym proto-r)
    (for-each (lambda (p)
		(when (pair? p)
		  (let loop ([l (car p)])
		    (cond
		     [(syntax? l)
		      (when (bound-identifier=? l ssym)
			(raise-syntax-error 
			 'syntax
			 "missing ellipses with pattern variable in template"
			 ssym))]
		     [else (loop (car l))]))))
	      proto-r))

  ;; Tests if x is an ellipsing pattern of the form
  ;;   (blah ... . blah2)
  (-define (ellipsis? x)
    (and (stx-pair? x) 
	 (let ([d (stx-cdr x)])
	   (and (stx-pair? d) 
		(...? (stx-car d))
		(not (...? (stx-car x)))))))

  ;; Takes an environment prototype and removes
  ;; the ellipsis-nesting information.
  (-define (flatten-nestings nestings filter?)
    (let loop ([nestings nestings])
      (if (null? nestings)
	  null
	  (if (filter? (car nestings))
	      (cons (let loop ([nesting (car nestings)])
		      (if (syntax? nesting)
			  nesting
			  (loop (car nesting))))
		    (loop (cdr nestings)))
	      (loop (cdr nestings))))))

  (-define (multiple-ellipsis-vars? proto-r)
    (let loop ([proto-r proto-r])
      (cond
       [(null? proto-r) #f]
       [(pair? (car proto-r))
	(let loop ([proto-r (cdr proto-r)])
	  (cond
	   [(null? proto-r) #f]
	   [(pair? (car proto-r))
	    #t]
	   [else (loop (cdr proto-r))]))]
       [else (loop (cdr proto-r))])))

  (-define (no-ellipses? stx)
    (cond
     [(stx-pair? stx)
      (and (no-ellipses? (stx-car stx))
	   (no-ellipses? (stx-cdr stx)))]
     [(identifier? stx)
      (not (...? stx))]
     [else #t]))

  ;; Structure for communicating first-order pattern variable information:
  (define-values (struct:syntax-mapping -make-syntax-mapping -syntax-mapping? syntax-mapping-ref syntax-mapping-set!)
    (make-struct-type 'syntax-mapping #f 2 0 #f null (current-inspector)
                      (lambda (self stx)
                        (if (identifier? stx)
                            (raise-syntax-error
                             #f
                             "pattern variable cannot be used outside of a template"
                             stx)
                            (raise-syntax-error
                             #f
                             "pattern variable cannot be used outside of a template"
                             stx
                             (if (module-template-identifier=? (quote-syntax set!) (stx-car stx))
                                 (stx-car (stx-cdr stx))
                                 (stx-car stx)))))))
  (-define -syntax-mapping-depth (make-struct-field-accessor syntax-mapping-ref 0))
  (-define -syntax-mapping-valvar (make-struct-field-accessor syntax-mapping-ref 1))
  (-define (make-syntax-mapping depth valvar)
    (make-set!-transformer (-make-syntax-mapping depth valvar)))
  (-define (syntax-mapping? v)
    (and (set!-transformer? v)
         (-syntax-mapping? (set!-transformer-procedure v))))
  (-define (syntax-mapping-depth v)
    (-syntax-mapping-depth (set!-transformer-procedure v)))
  (-define (syntax-mapping-valvar v)
    (-syntax-mapping-valvar (set!-transformer-procedure v)))

  (provide (protect make-match&env get-match-vars make-pexpand
		    make-syntax-mapping syntax-mapping?
		    syntax-mapping-depth syntax-mapping-valvar
		    stx-memq-pos no-ellipses?)))

;;----------------------------------------------------------------------
;; syntax-case and syntax

(module #%stxcase #%kernel
  (require #%stx #%small-scheme #%paramz)
  (require-for-syntax #%stx #%small-scheme #%sc #%kernel)

  (-define (datum->syntax-object/shape orig datum)
     (if (syntax? datum)
	 datum
	 (let ([stx (datum->syntax-object orig datum orig #f orig)])
	   (let ([shape (syntax-property orig 'paren-shape)])
	     (if shape
		 (syntax-property stx 'paren-shape shape)
		 stx)))))

  (-define (catch-ellipsis-error thunk sexp sloc)
      ((let/ec esc
	 (with-continuation-mark
	     exception-handler-key
             (lambda (exn)
               (esc
                (lambda ()
                  (if (exn:break? exn)
                      (raise exn)
                      (raise-syntax-error
                       'syntax
                       "incompatible ellipsis match counts for template"
                       sexp
                       sloc)))))
	   (let ([v (thunk)])
	     (lambda () v))))))

  (-define substitute-stop 'dummy)

  ;; pattern-substitute optimizes a pattern substitution by
  ;;  merging variables that look up the same simple mapping
  (-define-syntax pattern-substitute
    (lambda (stx)
      (let ([pat (stx-car (stx-cdr stx))]
	    [subs (stx->list (stx-cdr (stx-cdr stx)))])
	(let ([ht-common (make-hash-table 'equal)]
	      [ht-map (make-hash-table)])
	  ;; Determine merges:
	  (let loop ([subs subs])
	    (unless (null? subs)
	      (let ([id (syntax-e (car subs))]
		    [expr (cadr subs)])
		(when (or (identifier? expr)
			  (and (stx-pair? expr)
			       (memq (syntax-e (stx-car expr))
				     '(car cadr caddr cadddr
					   cdr cddr cdddr cddddr
					   list-ref list-tail))
			       (stx-pair? (stx-cdr expr))
			       (identifier? (stx-car (stx-cdr expr)))))
		  (let ([s-expr (syntax-object->datum expr)])
		    (let ([new-id (hash-table-get ht-common s-expr #f)])
		      (if new-id
			  (hash-table-put! ht-map id new-id)
			  (hash-table-put! ht-common s-expr id))))))
	      (loop (cddr subs))))
	  ;; Merge:
	  (let ([new-pattern (if (zero? (hash-table-count ht-map))
				 pat
				 (let loop ([stx pat])
				   (cond
				    [(pair? stx)
				     (let ([a (loop (car stx))]
					   [b (loop (cdr stx))])
				       (if (and (eq? a (car stx))
						(eq? b (cdr stx)))
					   stx
					   (cons a b)))]
				    [(symbol? stx)
				     (let ([new-id (hash-table-get ht-map stx #f)])
				       (or new-id stx))]
				    [(syntax? stx) 
				     (let ([new-e (loop (syntax-e stx))])
				       (if (eq? (syntax-e stx) new-e)
					   stx
					   (datum->syntax-object stx new-e stx stx)))]
				    [(vector? stx)
				     (list->vector (map loop (vector->list stx)))]
				    [(box? stx) (box (loop (unbox stx)))]
				    [else stx])))])
	    (datum->syntax-object (quote-syntax here)
				  `(apply-pattern-substitute
				    ,new-pattern
				    (quote ,(let loop ([subs subs])
					      (cond
					       [(null? subs) null]
					       [(hash-table-get ht-map (syntax-e (car subs)) #f)
						;; Drop mapped id
						(loop (cddr subs))]
					       [else
						(cons (car subs) (loop (cddr subs)))])))
				    . ,(let loop ([subs subs])
					 (cond
					  [(null? subs) null]
					  [(hash-table-get ht-map (syntax-e (car subs)) #f)
					   ;; Drop mapped id
					   (loop (cddr subs))]
					  [else
					   (cons (cadr subs) (loop (cddr subs)))])))
				  stx))))))

  (-define apply-pattern-substitute
     (lambda (stx sub-ids . sub-vals)
       (let loop ([stx stx])
	 (cond
	  [(pair? stx) (let ([a (loop (car stx))]
			     [b (loop (cdr stx))])
			 (if (and (eq? a (car stx))
				  (eq? b (cdr stx)))
			     stx
			     (cons a b)))]
	  [(symbol? stx)
	   (let sloop ([sub-ids sub-ids][sub-vals sub-vals])
	     (cond
	      [(null? sub-ids) stx]
	      [(eq? stx (car sub-ids)) (car sub-vals)]
	      [else (sloop (cdr sub-ids) (cdr sub-vals))]))]
	  [(syntax? stx) 
	   (let ([new-e (loop (syntax-e stx))])
	     (if (eq? (syntax-e stx) new-e)
		 stx
                 (datum->syntax-object/shape stx new-e)))]
	  [(vector? stx)
	   (list->vector (map loop (vector->list stx)))]
	  [(box? stx) (box (loop (unbox stx)))]
	  [else stx]))))

  (-define-syntax syntax-case**
    (lambda (x)
      (-define l (and (stx-list? x) (cdr (stx->list x))))
      (unless (and (stx-list? x)
		   (> (length l) 3))
	(raise-syntax-error
	 #f
	 "bad form"
	 x))
      (let ([who (car l)]
	    [arg-is-stx? (cadr l)]
	    [expr (caddr l)]
	    [kws (cadddr l)]
	    [lit-comp (cadddr (cdr l))]
	    [clauses (cddddr (cdr l))])
	(unless (stx-list? kws)
	  (raise-syntax-error
	   (syntax-e who)
	   "expected a parenthesized sequence of literal identifiers"
	   kws))
	(for-each
	 (lambda (lit)
	   (unless (identifier? lit)
	     (raise-syntax-error
	      (syntax-e who)
	      "literal is not an identifier"
	      lit)))
	 (stx->list kws))
	(for-each
	 (lambda (clause)
	   (unless (and (stx-list? clause)
			(<= 2 (length (stx->list clause)) 3))
	     (raise-syntax-error
	      (syntax-e who)
	      "bad clause"
	      clause)))
	 clauses)
	(let ([patterns (map stx-car clauses)]
	      [fenders (map (lambda (clause)
			      (and (stx-pair? (stx-cdr (stx-cdr clause)))
				   (stx-car (stx-cdr clause))))
			    clauses)]
	      [answers (map (lambda (clause)
			      (let ([r (stx-cdr (stx-cdr clause))])
				(if (stx-pair? r) 
				    (stx-car r)
				    (stx-car (stx-cdr clause)))))
			    clauses)])
	  (let* ([arg (quote-syntax arg)]
		 [rslt (quote-syntax rslt)]
		 [pattern-varss (map
				 (lambda (pattern)
				   (get-match-vars who pattern pattern (stx->list kws)))
				 (stx->list patterns))]
		 [lit-comp-is-mod? (and (identifier? lit-comp)
					(module-identifier=? 
					 lit-comp
					 (quote-syntax module-identifier=?)))])
	    (datum->syntax-object
	     (quote-syntax here)
	     (list (quote-syntax let) (list (list arg (if (syntax-e arg-is-stx?)
							  expr
							  (list (quote-syntax datum->syntax-object)
							    (list
							     (quote-syntax quote-syntax)
							     (datum->syntax-object
							      expr
							      'here))
							    expr))))
		   (let loop ([patterns patterns]
			      [fenders fenders]
			      [unflat-pattern-varss pattern-varss]
			      [answers answers])
		     (cond
		      [(null? patterns)
		       (list
			(quote-syntax raise-syntax-error)
			#f
			"bad syntax"
			arg)]
		      [else
		       (let ([rest (loop (cdr patterns) (cdr fenders)
					 (cdr unflat-pattern-varss) (cdr answers))])
			 (let ([pattern (car patterns)]
			       [fender (car fenders)]
			       [unflat-pattern-vars (car unflat-pattern-varss)]
			       [answer (car answers)])
			   (-define pattern-vars
			     (map (lambda (var)
				    (let loop ([var var])
				      (if (syntax? var)
					  var
					  (loop (car var)))))
				  unflat-pattern-vars))
			   (-define temp-vars
			     (map
			      (lambda (p) (datum->syntax-object p (gensym) #f))
			      pattern-vars))
			   (-define tail-pattern-var (sub1 (length pattern-vars)))
			   ;; Here's the result expression for one match:
			   (let* ([do-try-next (if (car fenders)
						   (list (quote-syntax try-next))
						   rest)]
				  [mtch (make-match&env
					 who
					 pattern
					 pattern
					 (stx->list kws)
					 (not lit-comp-is-mod?))]
				  [cant-fail? (if lit-comp-is-mod?
						  (equal? mtch '(lambda (e) e))
						  (equal? mtch '(lambda (e module-identifier=?) e)))]
				  [m
				   ;; Do match, bind result to rslt:
				   (list (quote-syntax let)
					 (list 
					  (list rslt
						(if cant-fail?
						    arg
						    (list* (datum->syntax-object
							    (quote-syntax here)
							    mtch
							    pattern)
							   arg
							   (if lit-comp-is-mod?
							       null
							       (list lit-comp))))))
					 ;; If match succeeded...
					 (list 
					  (quote-syntax if)
					  (if cant-fail?
					      #t
					      rslt)
					  ;; Extract each name binding into a temp variable:
					  (list
					   (quote-syntax let) 
					   (map (lambda (pattern-var temp-var)
						  (list
						   temp-var
						   (let ([pos (stx-memq-pos pattern-var pattern-vars)])
						     (let ([accessor (cond
								      [(= tail-pattern-var pos)
								       (cond
									[(eq? pos 0) 'tail]
									[(eq? pos 1) (quote-syntax cdr)]
									[(eq? pos 2) (quote-syntax cddr)]
									[(eq? pos 3) (quote-syntax cdddr)]
									[(eq? pos 4) (quote-syntax cddddr)]
									[else 'tail])]
								      [(eq? pos 0) (quote-syntax car)]
								      [(eq? pos 1) (quote-syntax cadr)]
								      [(eq? pos 2) (quote-syntax caddr)]
								      [(eq? pos 3) (quote-syntax cadddr)]
								      [else #f])])
						       (cond
							[(eq? accessor 'tail)
							 (if (zero? pos)
							     rslt
							     (list
							      (quote-syntax list-tail)
							      rslt
							      pos))]
							[accessor (list
								   accessor
								   rslt)]
							[else (list
							       (quote-syntax list-ref)
							       rslt
							       pos)])))))
						pattern-vars temp-vars)
					   ;; Tell nested `syntax' forms about the
					   ;;  pattern-bound variables:
					   (list
					    (quote-syntax letrec-syntaxes+values) 
					    (map (lambda (pattern-var unflat-pattern-var temp-var)
						   (list (list pattern-var)
							 (list
							  (quote-syntax make-syntax-mapping)
							  ;; Tell it the shape of the variable:
							  (let loop ([var unflat-pattern-var][d 0])
							    (if (syntax? var)
								d
								(loop (car var) (add1 d))))
							  ;; Tell it the variable name:
							  (list
							   (quote-syntax quote-syntax)
							   temp-var))))
						 pattern-vars unflat-pattern-vars
						 temp-vars)
					    null
					    (if fender
						(list (quote-syntax if) fender
						      answer
						      do-try-next)
						answer)))
					  do-try-next))])
			     (if fender
				 (list
				  (quote-syntax let)
				  ;; Bind try-next to try next case
				  (list (list (quote try-next)
					      (list (quote-syntax lambda)
						    (list)
						    rest)))
				  ;; Try one match
				  m)
				 ;; Match try already embed the rest case
				 m))))])))
	     x))))))

  (-define-syntax syntax
    (lambda (x)
      (-define here-stx (quote-syntax here))
      (unless (and (stx-pair? x)
		   (let ([rest (stx-cdr x)])
		     (and (stx-pair? rest)
			  (stx-null? (stx-cdr rest)))))
	(raise-syntax-error
	 #f
	 "bad form"
	 x))
      (datum->syntax-object
       here-stx
       (let ([pattern (stx-car (stx-cdr x))])
	 (let-values ([(unique-vars all-varss) (make-pexpand pattern #f null #f)])
	   (let ([var-bindings
		  (map
		   (lambda (var)
		     (and (let ([v (syntax-local-value var (lambda () #f))])
			    (and (syntax-mapping? v)
				 v))))
		   unique-vars)])
	     (if (and (or (null? var-bindings)
			  (not (ormap (lambda (x) x) var-bindings)))
		      (no-ellipses? pattern))
		 ;; Constant template:
		 (list (quote-syntax quote-syntax) pattern)
		 ;; Non-constant:
		 (let ([proto-r (let loop ([vars unique-vars][bindings var-bindings])
				  (if (null? bindings)
				      null
				      (let ([rest (loop (cdr vars)
							(cdr bindings))])
					(if (car bindings)
					    (cons (let loop ([v (car vars)]
							     [d (syntax-mapping-depth (car bindings))])
						    (if (zero? d)
							v
							(loop (list v) (sub1 d))))
						  rest)
					    rest))))]
		       [non-pattern-vars (let loop ([vars unique-vars][bindings var-bindings])
					   (if (null? bindings)
					       null
					       (let ([rest (loop (cdr vars)
								 (cdr bindings))])
						 (if (car bindings)
						     rest
						     (cons (car vars) rest)))))])
		   (let ([build-from-template
			  ;; Even if we don't use the builder, we need to check
			  ;; for a well-formed pattern:
			  (make-pexpand pattern proto-r non-pattern-vars pattern)]
			 [r (let loop ([vars unique-vars][bindings var-bindings][all-varss all-varss])
			      (cond
			       [(null? bindings) null]
			       [(car bindings)
                                (cons
                                 (syntax-property 
                                  (datum->syntax-object
                                   (car vars)
                                   (syntax-e (syntax-mapping-valvar (car bindings)))
                                   x)
                                  'disappeared-use
                                  (car all-varss))
                                 (loop (cdr vars) (cdr bindings) (cdr all-varss)))]
			       [else  (loop (cdr vars) (cdr bindings) (cdr all-varss))]))])
		     (if (identifier? pattern)
			 ;; Simple syntax-id lookup:
			 (car r)
			 ;; General case:
			 (list (datum->syntax-object
				here-stx
				build-from-template
				pattern)
			       (let ([len (length r)])
				 (cond
				  [(zero? len) (quote-syntax ())]
				  [(= len 1) (car r)]
				  [else
				   (cons (quote-syntax list*) r)]))))))))))
       x)))

  (provide syntax-case** syntax))

;;----------------------------------------------------------------------
;; syntax/loc

(module #%stxloc #%kernel
  (require #%qq-and-or #%stxcase #%define-et-al)
  (require-for-syntax #%kernel #%stxcase #%sc)

  ;; Regular syntax-case
  (-define-syntax syntax-case*
    (lambda (stx)
      (syntax-case** #f #t stx () module-identifier=?
	[(_ stxe kl id=? clause ...)
	 (syntax (syntax-case** _ #f stxe kl id=? clause ...))])))

  ;; Regular syntax-case
  (-define-syntax syntax-case
    (lambda (stx)
      (syntax-case** #f #t stx () module-identifier=?
	[(_ stxe kl clause ...)
	 (syntax (syntax-case** _ #f stxe kl module-identifier=? clause ...))])))

  (-define (relocate loc stx)
    (if (syntax-source loc)
        (datum->syntax-object stx
                              (syntax-e stx)
                              loc
                              #f
                              stx)
	stx))

  ;; Like syntax, but also takes a syntax object
  ;; that supplies a source location for the
  ;; resulting syntax object.
  (-define-syntax syntax/loc
    (lambda (stx)
      (syntax-case** #f #t stx () module-identifier=?
	[(_ loc pattern)
	 (if (if (symbol? (syntax-e #'pattern))
		 (syntax-mapping? (syntax-local-value #'pattern (lambda () #f)))
		 #f)
	     (syntax (syntax pattern))
	     (syntax (relocate loc (syntax pattern))))])))

  (provide syntax/loc syntax-case* syntax-case))

;;----------------------------------------------------------------------
;; with-syntax, generate-temporaries

(module #%with-stx #%kernel
  (require #%stx #%stxloc #%small-scheme #%stxcase)
  (require-for-syntax #%kernel #%stxcase #%stxloc #%sc #%qq-and-or #%cond)

  (-define (with-syntax-fail stx)
    (raise-syntax-error
     'with-syntax
     "binding match failed"
     stx))

  ;; Partly from Dybvig
  (-define-syntax with-syntax
    (lambda (x)
      (syntax-case x ()
	((_ () e1 e2 ...)
	 (syntax/loc x (begin e1 e2 ...)))
	((_ ((out in) ...) e1 e2 ...)
	 (let ([ins (syntax->list (syntax (in ...)))])
	   ;; Check for duplicates or other syntax errors:
	   (get-match-vars (syntax _) x (syntax (out ...)) null)
	   ;; Generate temps and contexts:
	   (let ([tmps (map (lambda (x) (gensym 'wstmp)) ins)]
		 [heres (map (lambda (x)
			       (datum->syntax-object
				x
				'here
				x))
			     ins)]
		 [outs (syntax->list (syntax (out ...)))])
	     ;; Let-bind RHSs, then build up nested syntax-cases:
	     (datum->syntax-object
	      #'here
	      `(let ,(map (lambda (tmp here in)
			    `[,tmp (datum->syntax-object 
				    (quote-syntax ,here) 
				    ,in)])
			  tmps heres ins)
		 ,(let loop ([tmps tmps][outs outs])
		    (cond
		     [(null? tmps)
		      (syntax (begin e1 e2 ...))]
		     [else `(syntax-case** #f #t ,(car tmps) () module-identifier=?
			      [,(car outs) ,(loop (cdr tmps)
						  (cdr outs))]
			      [_else (with-syntax-fail
				      ;; Minimize the syntax structure we keep:
				      (quote-syntax ,(datum->syntax-object 
						      #f 
						      (syntax-object->datum (car outs))
						      (car outs))))])])))
	      x)))))))

  (-define counter 0)
  (-define (append-number s)
    (set! counter (add1 counter))
    (string->symbol (format "~a~s" s counter)))

  (-define (generate-temporaries sl)
    (unless (stx-list? sl)
      (raise-type-error 
       'generate-temporaries
       "syntax pair"
       sl))
    (let ([l (stx->list sl)])
      (map (lambda (x) 
	     ((make-syntax-introducer)
	      (cond
	       [(symbol? x)
		(datum->syntax-object #f (append-number x))]
	       [(string? x)
		(datum->syntax-object #f (append-number x))]
	       [(identifier? x)
		(datum->syntax-object #f (append-number (syntax-e x)))]
	       [else 
		(datum->syntax-object #f (append-number 'temp))])))
	   l)))

  (provide with-syntax generate-temporaries))

;;----------------------------------------------------------------------
;; #%stxcase-scheme: adds let-syntax, syntax-rules, and
;;  check-duplicate-identifier, and assembles everything we have so far

(module #%stxcase-scheme #%kernel
  (require #%small-scheme #%stx #%stxcase #%with-stx #%stxloc)
  (require-for-syntax #%kernel #%small-scheme #%stx #%stxcase #%with-stx #%stxloc)

  (-define (check-duplicate-identifier names)
    (let/ec escape
      (let ([ht (make-hash-table)])
	(for-each
	 (lambda (defined-name)
	   (unless (identifier? defined-name)
	     (raise-type-error 'check-duplicate-identifier
			       "list of identifiers" names))
	   (let ([l (hash-table-get ht (syntax-e defined-name) null)])
	     (when (ormap (lambda (i) (bound-identifier=? i defined-name)) l)
	       (escape defined-name))
	     (hash-table-put! ht (syntax-e defined-name) (cons defined-name l))))
	 names)
	#f)))
  
  (-define-syntax letrec-syntaxes
    (lambda (stx)
      (syntax-case stx ()
	[(_ ([(id ...) expr] ...) body1 body ...)
	 (syntax/loc stx
	     (letrec-syntaxes+values ([(id ...) expr] ...)
				     ()
	       body1 body ...))])))

  (-define-syntax letrec-syntax
    (lambda (stx)
      (syntax-case stx ()
	[(_ ([id expr] ...) body1 body ...)
	 (syntax/loc stx
	     (letrec-syntaxes+values ([(id) expr] ...)
				     ()
	       body1 body ...))])))

  (-define-syntax let-syntaxes
    (lambda (stx)
      (syntax-case stx ()
	[(_ ([(id ...) expr] ...) body1 body ...)
	 (with-syntax ([((tmp ...) ...) 
			(map
			 generate-temporaries 
			 (syntax->list (syntax ((id ...) ...))))])
	   (syntax/loc stx
	       (letrec-syntaxes+values ([(tmp ...) expr] ...) ()
		 (letrec-syntaxes+values ([(id ...) (values
						     (make-rename-transformer (quote-syntax tmp))
						     ...)] ...)
					 ()
		   body1 body ...))))])))

  (-define-syntax let-syntax
    (lambda (stx)
      (syntax-case stx ()
	[(_ ([id expr] ...) body1 body ...)
	 (syntax/loc stx
	     (let-syntaxes ([(id) expr] ...)
	       body1 body ...))])))

  ;; From Dybvig, mostly:
  (-define-syntax syntax-rules
    (lambda (stx)
      (syntax-case** syntax-rules #t stx () module-identifier=?
	((_ (k ...) ((keyword . pattern) template) ...)
	 (andmap identifier? (syntax->list (syntax (k ...))))
	 (with-syntax (((dummy ...)
			(map (lambda (id)
			       (unless (identifier? id)
				 (raise-syntax-error
				  #f
				  "pattern must start with an identifier, found something else"
				  stx
				  id))
			       ;; Preserve the name, in case it's printed out
			       (string->uninterned-symbol (symbol->string (syntax-e id))))
			     (syntax->list (syntax (keyword ...))))))
	   (syntax/loc stx
	     (lambda (x)
	       (syntax-case** _ #t x (k ...) module-identifier=?
		 ((dummy . pattern) (syntax/loc x template))
		 ...))))))))

  (-define-syntax syntax-id-rules
    (lambda (x)
      (syntax-case** syntax-id-rules #t x () module-identifier=?
	((_ (k ...) (pattern template) ...)
	 (andmap identifier? (syntax->list (syntax (k ...))))
	 (syntax/loc x
	   (make-set!-transformer
	    (lambda (x)
	      (syntax-case** _ #t x (k ...) module-identifier=?
		(pattern (syntax/loc x template))
		...))))))))

  (provide syntax (all-from #%small-scheme)
	   (all-from #%with-stx) (all-from #%stxloc) check-duplicate-identifier
	   letrec-syntaxes letrec-syntax let-syntaxes let-syntax 
	   syntax-rules syntax-id-rules))

;;----------------------------------------------------------------------
;; #%qqstx : quasisyntax

(module #%qqstx #%kernel
  (require  #%stxcase-scheme #%stx)
  (require-for-syntax #%kernel #%stxcase-scheme #%stx)

  (provide quasisyntax
	   quasisyntax/loc
           unsyntax
           unsyntax-splicing)
  
  (define-syntaxes (unsyntax unsyntax-splicing)
    (let ([f (lambda (stx)
	       (raise-syntax-error
		#f
		"illegal outside of quasisyntax"
		stx))])
      (values f f)))

  (-define (check-splicing-list l ctx)
    (unless (stx-list? l)
      (raise-type-error
       'unsyntax-splicing
       "proper syntax list"
       l))
    (datum->syntax-object ctx l ctx))

  (define-syntaxes (quasisyntax quasisyntax/loc)
    (let ([qq
	   (lambda (orig-stx body mk-final)
	     (let ([here-stx #'here])
	       (let loop ([stx body]
			  [depth 0]
			  [same-k (lambda ()
				    (datum->syntax-object 
				     here-stx
				     (mk-final body)
				     orig-stx))]
			  [convert-k (lambda (body bindings)
				       (datum->syntax-object 
					here-stx
					(list
					 (quote-syntax with-syntax)
					 bindings
					 (mk-final body))
					orig-stx))])
		 (syntax-case stx (unsyntax unsyntax-splicing quasisyntax)
		   [(unsyntax x)
		    (if (zero? depth)
			(let ([temp (car (generate-temporaries '(uq)))])
			  (convert-k temp (list (list temp (syntax x)))))
			(loop (syntax x) (sub1 depth)
			      same-k
			      (lambda (v bindings)
				(convert-k (datum->syntax-object
					    here-stx
					    (list (stx-car stx) v)
					    stx)
					   bindings))))]
		   [unsyntax
		    (raise-syntax-error
		     #f
		     "misuse within quasisyntax"
		     orig-stx
		     stx)]
		   [((unsyntax-splicing x) . rest)
		    (if (zero? depth)
			(let ([rest-done-k
			       (lambda (rest-v bindings)
				 (with-syntax ([temp (car (generate-temporaries '(uqs)))]
					       [ctx (datum->syntax-object #'x 'ctx #'x)])
				   (convert-k (datum->syntax-object
					       stx
					       (list* (syntax temp)
						      (quote-syntax ...)
						      rest-v)
					       stx)
					      (cons #'[(temp (... ...)) (check-splicing-list x (quote-syntax ctx))]
						    bindings))))])
			  (loop (syntax rest) depth
				(lambda ()
				  (rest-done-k (syntax rest) null))
				rest-done-k))
			(let ([mk-rest-done-k
			       (lambda (x-v x-bindings)
				 (lambda (rest-v rest-bindings)
				   (convert-k (datum->syntax-object
					       stx
					       (cons x-v rest-v)
					       stx)
					      (append x-bindings
						      rest-bindings))))])
			  (loop (syntax x) (sub1 depth)
				(lambda ()
				  ;; x is unchanged.
				  (loop (syntax rest) depth
					same-k
					(mk-rest-done-k (stx-car stx) null)))
				(lambda (x-v x-bindings)
				  ;; x is generated by x-v
				  (let ([rest-done-k (mk-rest-done-k 
						      (datum->syntax-object
						       (stx-car stx)
						       (list (stx-car (stx-car stx)) x-v)
						       (stx-car stx))
						      x-bindings)])
				    (loop (syntax rest) depth
					  (lambda ()
					    ;; rest is unchanged
					    (rest-done-k (syntax rest) null))
					  rest-done-k))))))]
		   [unsyntax-splicing
		    (raise-syntax-error
		     #f
		     "misuse within quasisyntax"
		     orig-stx
		     stx)]
		   [(quasisyntax x)
		    (loop (syntax x) (add1 depth)
			  same-k
			  (lambda (v bindings)
			    (convert-k (datum->syntax-object
					stx
					(list (stx-car stx) v)
					stx)
				       bindings)))]
		   [_else
		    (cond
		     ;; We treat pairs specially so that we don't generate a lot
		     ;;  of syntax objects when the input syntax collapses a list
		     ;;  into a single syntax object.
		     [(pair? (syntax-e stx))
		      (let ploop ([l (syntax-e stx)]
				  [same-k same-k]
				  [convert-k (lambda (l bindings)
					       (convert-k (datum->syntax-object
							   stx
							   l
							   stx)
							  bindings))])
			(cond
			 [(pair? l)
			  (if (let ([a (car l)])
				(or (and (identifier? a)
					 (or (module-identifier=? a (quote-syntax unsyntax))
					     (module-identifier=? a (quote-syntax quasisyntax))))
				    (and (stx-pair? a)
					 (let ([a (stx-car a)])
					   (and (identifier? a)
						(module-identifier=? a (quote-syntax unsyntax-splicing)))))))
			      ;; Found something important, like `unsyntax'; stop the special
			      ;; handling for pairs
			      (loop (datum->syntax-object #f l #f) depth
				    same-k 
				    convert-k)
			      ;; Normal special pair handling
			      (ploop (cdr l)
				     (lambda ()
				       ;; rest is the same
				       (loop (car l) depth
					     same-k
					     (lambda (a a-bindings)
					       (convert-k (cons (datum->syntax-object
								 (car l)
								 a 
								 (car l))
								(cdr l))
							  a-bindings))))
				     (lambda (rest rest-bindings)
				       (loop (car l) depth
					     (lambda ()
					       (convert-k (cons (car l) rest)
							  rest-bindings))
					     (lambda (a a-bindings)
					       (convert-k (cons (datum->syntax-object
								 (car l)
								 a
								 (car l))
								rest)
							  (append a-bindings
								  rest-bindings)))))))]
			 [(null? l) (same-k)]
			 [else (loop l depth same-k convert-k)]))]
		     [(vector? (syntax-e stx))
		      (loop (datum->syntax-object
			     stx
			     (vector->list (syntax-e stx))
			     stx)
			    depth
			    same-k
			    (lambda (v bindings)
			      (convert-k (datum->syntax-object
					  stx
					  (list->vector (syntax->list v))
					  stx)
					 bindings)))]
		     [else
		      (same-k)])]))))])
      (values (lambda (orig-stx)
		(syntax-case orig-stx ()
		  [(_ stx) (qq orig-stx
			       (syntax stx) 
			       (lambda (body)
				 (list (quote-syntax syntax) body)))]))
	      (lambda (orig-stx)
		(syntax-case orig-stx ()
		  [(_ loc stx) (qq orig-stx
				   (syntax stx) 
				   (lambda (body)
				     (list (quote-syntax syntax/loc) 
					   (syntax loc)
					   body)))]))))))

;;----------------------------------------------------------------------
;; #%define : define and define-syntax

(module #%define #%kernel
  (require-for-syntax #%kernel #%stxcase-scheme #%stx #%qqstx)

  (provide define define-syntax define-for-syntax begin-for-syntax)

  (define-syntaxes (define define-syntax define-for-syntax)
    (let ([mk
	   (lambda (define-values-stx)
	     (lambda (stx)
	       (when (memq (syntax-local-context) '(expression))
		 (raise-syntax-error 
		  #f
		  "not allowed in an expression context"
		  stx))
	       (syntax-case stx ()
		 [(_ id expr)
		  (identifier? #'id)
		  (quasisyntax/loc stx (#,define-values-stx (id) expr))]
		 [(_ id . rest)
		  (identifier? #'id)
		  (raise-syntax-error
		   #f
		   (syntax-case stx ()
		     [(_ id)
		      "bad syntax (zero expressions after identifier)"]
		     [(_ id expr ...)
		      "bad syntax (multiple expressions after identifier)"]
		     [(_ id . rest)
		      "bad syntax (illegal use of `.')"])
		   stx)]
		 [(_ something . rest)
		  (not (stx-pair? #'something))
		  (raise-syntax-error
		   #f
		   "bad syntax"
		   stx
		   #'something)]
		 [(_ proto . body)
		  (let-values ([(id mk-rhs)
				(letrec ([simple-proto
					  ;; check the args and set up a proc-maker; we return
					  ;;  a proc maker instead of a final proc to enable
					  ;;  left-to-right checking of the function protos
					  (lambda (proto)
					    (let-values ([(args mk-rhs)
							  (syntax-case proto ()
							    [(id arg ...)
							     (values (syntax->list #'(arg ...))
								     (lambda (body)
								       (quasisyntax/loc stx (lambda (arg ...)
											      . #,body))))]
							    [(id arg ... . rest)
							     (values (syntax->list #'(arg ... rest))
								     (lambda (body)
								       (quasisyntax/loc stx 
									 (lambda (arg ... . rest)
									   . #,body))))])])
					      (for-each (lambda (a)
							  (unless (identifier? a)
							    (raise-syntax-error
							     #f
							     "not an identifier for procedure argument"
							     stx
							     a)))
							args)
					      (let ([dup (check-duplicate-identifier args)])
						(when dup
						  (raise-syntax-error
						   #f
						   "duplicate argument identifier"
						   stx
						   dup)))
					      mk-rhs))]
					 [general-proto
					  ;; proto is guaranteed to be a stx-pair
					  (lambda (proto)
					    (syntax-case proto ()
					      [(id . rest)
					       (identifier? #'id)
					       (values #'id
						       (simple-proto proto))]
					      [((something . more) . rest)
					       ;; Here's where left-to-right checking comes in:
					       ;;  first check the (something . more), then
					       ;;  the rest.
					       (let-values ([(id mk-rhs) (general-proto #'(something . more))])
						 (let ([mk-inner (simple-proto proto)])
						   (values id
							   (lambda (body)
							     (mk-rhs (list (mk-inner body)))))))]
					      [(other . rest)
					       (raise-syntax-error
						#f
						"bad syntax (not an identifier for procedure name, and not a nested procedure form)"
						stx
						#'other)]))])
				  (general-proto #'proto))])
		    (unless (stx-list? #'body)
		      (raise-syntax-error
		       #f
		       "bad syntax (illegal use of `.' for procedure body)"
		       stx))
		    (when (stx-null? #'body)
		      (raise-syntax-error
		       #f
		       "bad syntax (no expressions for procedure body)"
		       stx))
		    (quasisyntax/loc stx (#,define-values-stx (#,id) #,(mk-rhs #'body))))])))])
      (values (mk #'define-values)
	      (mk #'define-syntaxes)
	      (mk #'define-values-for-syntax))))

  (define-syntaxes (begin-for-syntax)
    (lambda (stx)
      (let ([ctx (syntax-local-context)])
	(unless (memq ctx '(module module-begin top-level))
	  (raise-syntax-error #f "allowed only at the top-level or a module top-level" stx))
	(syntax-case stx ()
	  [(_) #'(begin)]
	  [(_ elem)
	   (not (eq? ctx 'module-begin))
	   (let ([e (local-transformer-expand/capture-lifts
		     #'elem
		     ctx
		     (syntax->list
		      #'(begin
			  define-values
			  define-syntaxes
			  define-values-for-syntax
			  set!
			  let-values
			  let*-values
			  letrec-values
			  lambda
			  case-lambda
			  if
			  quote
			  letrec-syntaxes+values
			  fluid-let-syntax
			  with-continuation-mark
                          #%expression
                          #%variable-reference
			  #%app
			  #%top
			  #%datum
                          provide 
                          require
                          require-for-syntax
                          require-for-template)))])
	     (syntax-case* e (begin define-values define-syntaxes require require-for-template) 
			   module-transformer-identifier=?
	       [(begin v ...)
		#'(begin-for-syntax v ...)]
	       [(define-values (id ...) expr)
		#'(define-values-for-syntax (id ...) expr)]
	       [(require v ...)
		#'(require-for-syntax v ...)]
	       [(require-for-template v ...)
		#'(require v ...)]
	       [(define-syntaxes (id ...) expr)
		(raise-syntax-error
		 #f
		 "syntax definitions not allowed within begin-for-syntax"
		 #'elem)]
	       [other 
		#'(define-values-for-syntax () (begin other (values)))]))]
	  [(_ elem ...)
	   ;; We split up the elems so that someone else can
	   ;;  worry about the fact that properly expanding the second
	   ;;  things might depend somehow on the first thing.
	   ;; This also avoids a problem when `begin-for-syntax' is the
	   ;;  only thing in a module body, and `module' has to expand
	   ;;  it looking for #%module-begin.
	   (syntax/loc stx (begin (begin-for-syntax elem) ...))])))))

;;----------------------------------------------------------------------
;; #%more-scheme : case, do, etc. - remaining syntax

(module #%more-scheme #%kernel
  (require #%small-scheme #%define #%paramz)
  (require-for-syntax #%kernel #%stx #%stxcase-scheme #%qqstx)

  (define-syntax case-test
    (lambda (x)
      (syntax-case x ()
	[(_ x (k))
	 (if (symbol? (syntax-e #'k))
	     (syntax (eq? x 'k))
	     (syntax (eqv? x 'k)))]
	[(_ x (k ...))
	 (syntax (memv x '(k ...)))])))

  ;; From Dybvig:
  (define-syntax case
    (lambda (x)
      (syntax-case x (else)
	((_ v)
	 (syntax (begin v (cond))))
	((_ v (else e1 e2 ...))
	 (syntax/loc x (begin v e1 e2 ...)))
	((_ v ((k ...) e1 e2 ...))
	 (syntax/loc x (if (case-test v (k ...)) (begin e1 e2 ...))))
	((_ v ((k ...) e1 e2 ...) c1 c2 ...)
	 (syntax/loc x (let ((x v))
			 (if (case-test x (k ...))
			     (begin e1 e2 ...)
			     (case x c1 c2 ...)))))
	((_ v (bad e1 e2 ...) . rest)
	 (raise-syntax-error 
	  #f
	  "bad syntax (not a datum sequence)"
	  x
	  (syntax bad)))
	((_ v clause . rest)
	 (raise-syntax-error 
	  #f
	  "bad syntax (missing expression after datum sequence)"
	  x
	  (syntax clause)))
	((_ . v)
	 (not (null? (syntax-e (syntax v))))
	 (raise-syntax-error 
	  #f
	  "bad syntax (illegal use of `.')"
	  x)))))

  ;; From Dybvig:
  (define-syntax do
    (lambda (orig-x)
      (syntax-case orig-x ()
	((_ ((var init . step) ...) (e0 e1 ...) c ...)
	 (with-syntax (((step ...)
			(map (lambda (v s)
			       (syntax-case s ()
				 (() v)
				 ((e) (syntax e))
				 (_ (raise-syntax-error 
				     #f
				     "bad variable syntax"
				     orig-x))))
			     (syntax->list (syntax (var ...)))
			     (syntax->list (syntax (step ...))))))
	   (syntax-case (syntax (e1 ...)) ()
	     (() (syntax/loc
		  orig-x
		  (let doloop ((var init) ...)
		    (if (not e0)
			(begin c ... (doloop step ...))))))
	     ((e1 e2 ...)
	      (syntax/loc
	       orig-x
	       (let doloop ((var init) ...)
		 (if e0
		     (begin e1 e2 ...)
		     (begin c ... (doloop step ...))))))))))))
  
  ;; From Dybvig:
  (define-syntax delay
    (lambda (x)
      (syntax-case x ()
	((delay exp)
	 (syntax/loc x (make-promise (lambda () exp)))))))
  
  (define-struct promise (p))
  
  (define (force p)
    (unless (promise? p)
      (raise-type-error 'force "promise" p))
    (let ([v (promise-p p)])
      (if (procedure? v)
	  (let ([v (call-with-values v list)])
	    (when (procedure? (promise-p p))
	      (set-promise-p! p v))
	    (apply values (promise-p p)))
	  (apply values v))))

  (define-syntax parameterize
    (lambda (stx)
      (syntax-case stx ()
	[(_ () expr1 expr ...)
	 (syntax (let () expr1 expr ...))]
	[(_ ([param val] ...) expr1 expr ...)
	 (with-syntax ([(p/v ...)
			(apply append
			       (map list
				    (syntax->list #'(param ...))
				    (syntax->list #'(val ...))))])
	   (syntax/loc stx
	     (with-continuation-mark
		 parameterization-key
		 (extend-parameterization
		  (continuation-mark-set-first #f parameterization-key)
		  p/v ...)
	       (let ()
		 expr1
		 expr ...))))])))

  (define-syntax parameterize*
    (syntax-rules ()
      [(_ () body1 body ...)
       (let () body1 body ...)]
      [(_ ([lhs1 rhs1] [lhs rhs] ...) body1 body ...)
       (parameterize ([lhs1 rhs1])
         (parameterize* ([lhs rhs] ...)
                        body1 body ...))]))

  (define (current-parameterization)
    (extend-parameterization (continuation-mark-set-first #f parameterization-key)))
  
  (define (call-with-parameterization paramz thunk)
    (unless (parameterization? paramz)
      (raise-type-error 'call-with-parameterization "parameterization" 0 paramz thunk))
    (unless (and (procedure? thunk)
		 (procedure-arity-includes? thunk 0))
      (raise-type-error 'call-with-parameterization "procedure (arity 0)" 1 paramz thunk))
    (with-continuation-mark
	parameterization-key
	paramz
      (thunk)))

  (define-syntax parameterize-break
    (lambda (stx)
      (syntax-case stx ()
	[(_ bool-expr expr1 expr ...)
	 (syntax/loc stx
	   (with-continuation-mark
	       break-enabled-key
	       (make-thread-cell (and bool-expr #t))
	     (begin
	       (check-for-break)
	       (let ()
		 expr1
		 expr ...))))])))
  
  (define-values (struct:break-paramz make-break-paramz break-paramz? break-paramz-ref break-paramz-set!)
    (make-struct-type 'break-parameterization #f 1 0 #f))

  (define-struct break-parameterization (cell))
  
  (define (current-break-parameterization)
    (make-break-paramz (continuation-mark-set-first #f break-enabled-key)))
  
  (define (call-with-break-parameterization paramz thunk)
    (unless (break-paramz? paramz)
      (raise-type-error 'call-with-break-parameterization "break parameterization" 0 paramz thunk))
    (unless (and (procedure? thunk)
		 (procedure-arity-includes? thunk 0))
      (raise-type-error 'call-with-parameterization "procedure (arity 0)" 1 paramz thunk))
    (begin0
     (with-continuation-mark
	 break-enabled-key
	 (break-paramz-ref paramz 0)
       (begin
	 (check-for-break)
	 (thunk)))
     (check-for-break)))

  (define (select-handler/no-breaks e bpz l)
    (cond
     [(null? l)
      (raise e)]
     [((caar l) e)
      (begin0
       ((cdar l) e)
       (with-continuation-mark 
	   break-enabled-key
	   bpz
	 (check-for-break)))]
     [else
      (select-handler/no-breaks e bpz (cdr l))]))

  (define (select-handler/breaks-as-is e bpz l)
    (cond
     [(null? l)
      (raise e)]
     [((caar l) e)
      (with-continuation-mark 
	  break-enabled-key
	  bpz
	(begin
	  (check-for-break)
	  ((cdar l) e)))]
     [else
      (select-handler/breaks-as-is e bpz (cdr l))]))

  (define false-thread-cell (make-thread-cell #f))


  (define (check-with-handlers-in-context handler-prompt-key)
    (unless (continuation-prompt-available? handler-prompt-key) 
      (error 'with-handlers
             "exception handler used out of context")))

  (define handler-prompt-key (make-continuation-prompt-tag))

  (define-syntaxes (with-handlers with-handlers*)
    (let ([wh 
	   (lambda (disable-break?)
	     (lambda (stx)
	       (syntax-case stx ()
		 [(_ () expr1 expr ...) (syntax/loc stx (let () expr1 expr ...))]
		 [(_ ([pred handler] ...) expr1 expr ...)
		  (with-syntax ([(pred-name ...) (generate-temporaries (map (lambda (x) 'with-handlers-predicate) 
									    (syntax->list #'(pred ...))))]
				[(handler-name ...) (generate-temporaries (map (lambda (x) 'with-handlers-handler) 
									       (syntax->list #'(handler ...))))])
		    (quasisyntax/loc stx
		      (let ([pred-name pred] ...
			    [handler-name handler] ...)
			;; Capture current break parameterization, so we can use it to
			;;  evaluate the body
			(let ([bpz (continuation-mark-set-first #f break-enabled-key)])
			  ;; Disable breaks here, so that when the exception handler jumps
			  ;;  to run a handler, breaks are disabled for the handler
			  (with-continuation-mark
			      break-enabled-key
			      false-thread-cell
			    (call-with-continuation-prompt
			     (lambda ()
			       ;; Restore the captured break parameterization for
			       ;;  evaluating the `with-handlers' body. In this
			       ;;  special case, no check for breaks is needed,
			       ;;  because bpz is quickly restored past call/ec.
			       ;;  Thus, `with-handlers' can evaluate its body in
			       ;;  tail position.
			       (with-continuation-mark 
				   break-enabled-key
				   bpz
                                 (with-continuation-mark 
                                     exception-handler-key
                                     (lambda (e)
                                       ;; Deliver a thunk to the escape handler:
                                       (abort-current-continuation
                                        handler-prompt-key
                                        (lambda ()
                                          (#,(if disable-break?
                                                 #'select-handler/no-breaks
                                                 #'select-handler/breaks-as-is)
                                           e bpz
                                           (list (cons pred-name handler-name) ...)))))
                                   (let ()
                                     expr1 expr ...))))
                             handler-prompt-key
			     ;; On escape, apply the handler thunk
			     (lambda (thunk) (thunk))))))))])))])
      (values (wh #t) (wh #f))))

  (define (call-with-exception-handler exnh thunk)
    (with-continuation-mark
        exception-handler-key
        exnh
      (thunk)))

  (define-syntax set!-values
    (lambda (stx)
      (syntax-case stx ()
	[(_ () expr) (syntax (let-values ([() expr]) (void)))]
	[(_ (id) expr) (identifier? (syntax id)) (syntax (set! id expr))]
	[(_ (id ...) expr)
	 (let ([ids (stx->list (syntax (id ...)))])
	   (for-each
	    (lambda (id)
	      (unless (identifier? id)
		(raise-syntax-error #f
				    "not an identifier"
				    stx
				    id)))
	    ids)
	   (let ([dup (check-duplicate-identifier ids)])
	     (when dup
	       (raise-syntax-error #f
				   "duplicate identifier"
				   stx
				   dup))))
	 (with-syntax ([(temp ...) (generate-temporaries (syntax (id ...)))])
	   (syntax/loc
	    stx
	    (let-values ([(temp ...) expr])
	      (set! id temp) ...)))])))

  (define-syntax let/cc
    (lambda (stx)
      (syntax-case stx ()
	[(_ var body1 body ...)
	 (syntax/loc stx (call/cc (lambda (var) body1 body ...)))])))

  (define-syntax let-struct
    (lambda (stx)
      (syntax-case stx ()
	[(_ base (field ...) body1 body ...)
	 (syntax/loc stx (let ()
			   (define-struct base (field ...))
			   body1 body ...))])))

  (define-syntax fluid-let
    (lambda (stx)
      (syntax-case stx ()
	[(_ () body1 body ...) (syntax/loc stx (let () body1 body ...))]
	[(_ ([name val] ...) body1 body ...)
	 (with-syntax ([(tmp ...) (generate-temporaries (syntax (name ...)))])
	   (syntax/loc
	    stx
	    (let ([tmp val] ...)
	      (let ([swap
		     (lambda ()
		       (let ([s tmp])
			 (set! tmp name)
			 (set! name s))
		       ...)])
		(dynamic-wind
		    swap
		    (lambda () body1 body ...)
		    swap)))))])))

  (define-syntax time
    (lambda (stx)
      (syntax-case stx ()
	[(_ expr1 expr ...)
	 (syntax/loc
	  stx
	  (let-values ([(v cpu user gc) (time-apply (lambda () expr1 expr ...) null)])
	    (printf "cpu time: ~s real time: ~s gc time: ~s~n" cpu user gc)
	    (apply values v)))])))

  (provide case do delay force promise?
	   parameterize parameterize* current-parameterization call-with-parameterization
	   parameterize-break current-break-parameterization call-with-break-parameterization
	   with-handlers with-handlers* call-with-exception-handler
           set!-values
	   let/cc let-struct fluid-let time))

;;----------------------------------------------------------------------
;; #%misc : file utilities, etc. - remaining functions

(module #%misc #%kernel
  (require #%more-scheme #%small-scheme #%memtrace #%define)
  (require-for-syntax #%kernel #%stx #%stxcase-scheme)

  (define (path-string? s)
    (or (path? s) 
	(and (string? s)
	     (or (relative-path? s)
		 (absolute-path? s)))))

  (define -re:suffix #rx#"([.][^.]*|)$")
  (define (path-replace-suffix s sfx)
    (unless (or (path-for-some-system? s)
                (path-string? s))
      (raise-type-error 'path-replace-suffix "path (for any system) or valid-path string" 0 s sfx))
    (unless (or (string? sfx) (bytes? sfx))
      (raise-type-error 'path-replace-suffix "string or byte string" 1 s sfx))
    (let-values ([(base name dir?) (split-path s)])
      (when (not base)
	(raise-mismatch-error 'path-replace-suffix "cannot add a suffix to a root path: " s))
      (let ([new-name (bytes->path-element
		       (regexp-replace -re:suffix 
				       (path-element->bytes name)
				       (if (string? sfx)
					   (string->bytes/locale sfx (char->integer #\?))
					   sfx))
                       (if (path-for-some-system? s)
                           (path-convention-type s)
                           (system-path-convention-type)))])
	(if (path? base)
	    (build-path base new-name)
	    new-name))))

  (define bsbs (string #\u5C #\u5C))

  (define (normal-case-path s)
    (unless (or (path-for-some-system? s)
                (path-string? s))
      (raise-type-error 'normal-path-case "path (for any system) or valid-path string" s))
    (cond
     [(if (path-for-some-system? s)
          (eq? (path-convention-type s) 'windows)
          (eq? (system-type) 'windows))
      (let ([str (if (string? s) s (bytes->string/locale (path->bytes s)))])
	(if (regexp-match? #rx"^[\u5C][\u5C][?][\u5C]" str)
	    (if (string? s)
		(string->path s)
		s)
	    (let ([s (string-locale-downcase str)])
	      (bytes->path 
               (string->bytes/locale
                (regexp-replace* #rx"/" 
                                 (if (regexp-match? #rx"[/\u5C][. ]+[/\u5C]*$" s)
                                     ;; Just "." or ".." in last path element - don't remove
                                     s
                                     (regexp-replace* #rx"\u5B .\u5D+([/\u5C]*)$" s "\u005C1"))
                                 bsbs))
               'windows))))]
     [(string? s) (string->path s)]
     [else s]))

  (define rationalize
    (letrec ([check (lambda (x) 
                      (unless (real? x) (raise-type-error 'rationalize "real" x)))]
	     [find-between 
	      (lambda (lo hi)
		(if (integer? lo)
		    lo
		    (let ([lo-int (floor lo)]
			  [hi-int (floor hi)])
		      (if (< lo-int hi-int)
			  (add1 lo-int)
			  (+ lo-int
			     (/ (find-between (/ (- hi lo-int)) (/ (- lo lo-int)))))))))])
      (lambda (x within)
	(check x) (check within)
	(let* ([delta (abs within)]
	       [lo (- x delta)]
	       [hi (+ x delta)])
	  (cond
	   [(not (= x x)) +nan.0]
	   [(<= lo 0 hi) (if (exact? x) 0 0.0)]
	   [(negative? lo) (- (find-between (- hi) (- lo)))]
	   [else (find-between lo hi)])))))

  (define (read-eval-print-loop)
    (let repl-loop ()
      ;; This prompt catches all error escapes, including from read and print.
      (call-with-continuation-prompt
       (lambda ()
         (let ([v ((current-prompt-read))])
           (unless (eof-object? v)
             (call-with-values
                 (lambda () 
                   ;; This prompt catches escapes during evaluation.
                   ;; Unlike the outer prompt, the handler prints
                   ;; the results.
                   (call-with-continuation-prompt
                    (lambda ()
                      (let ([w (cons '#%top-interaction v)])
                        ((current-eval) (if (syntax? v)
                                            (namespace-syntax-introduce 
                                             (datum->syntax-object #f w v))
                                            w))))))
               (lambda results (for-each (current-print) results)))
             ;; Abort to loop. (Calling `repl-loop' directory would not be a tail call.)
             (abort-current-continuation (default-continuation-prompt-tag)))))
       (default-continuation-prompt-tag)
       (lambda args (repl-loop)))))

  (define load/cd
    (lambda (n)
      (unless (path-string? n)
	(raise-type-error 'load/cd "path or string (sans nul)" n))
      (let-values ([(base name dir?) (split-path n)])
	(if dir?
	    (raise
	     (make-exn:fail:filesystem
	      (string->immutable-string
	       (format "load/cd: cannot open a directory: ~s" n))
	      (current-continuation-marks)))
	    (if (not (path? base))
		(load n)
		(begin
		  (if (not (directory-exists? base))
		      (raise
		       (make-exn:fail:filesystem
			(string->immutable-string
			 (format 
			  "load/cd: directory of ~s does not exist (current directory is ~s)" 
			  n (current-directory)))
			(current-continuation-marks))))
		  (let ([orig (current-directory)])
		    (dynamic-wind
			(lambda () (current-directory base))
			(lambda () (load name))
			(lambda () (current-directory orig))))))))))

  (define (-load load name path)
    (unless (path-string? path) 
      (raise-type-error name "path or string (sans nul)" path))
    (if (complete-path? path)
	(load path)
	(let ([dir (current-load-relative-directory)])
	  (load (if dir (path->complete-path path dir) path)))))
  (define (load-relative path) (-load load 'load-relative path))
  (define (load-relative-extension path) (-load load-extension 'load-relative-extension path))
  
  (define path-list-string->path-list
    (let ((r (byte-regexp (string->bytes/utf-8
			   (let ((sep (case (system-type) 
					((unix beos oskit macosx) ":")
					((windows macos) ";"))))
			     (format "([^~a]*)~a(.*)" sep sep)))))
	  (cons-path (lambda (default s l) 
		       (if (bytes=? s #"")
			   (append default l)
			     (cons (bytes->path s) l)))))
      (lambda (s default)
	(unless (or (bytes? s)
		    (string? s))
	  (raise-type-error 'path-list-string->path-list "byte string or string" s))
	(unless (and (list? default)
		     (andmap path? default))
	  (raise-type-error 'path-list-string->path-list "list of paths" default))
	(let loop ([s (if (string? s)
			  (string->bytes/utf-8 s)
			  s)])
	  (let ([m (regexp-match r s)])
	    (if m
		(cons-path default (cadr m) (loop (caddr m)))
		(cons-path default s null)))))))

  (define find-executable-path
    (case-lambda 
     [(program libpath reverse?)
      (unless (path-string? program) 
	(raise-type-error 'find-executable-path "path or string (sans nul)" program))
      (unless (or (not libpath) (and (path-string? libpath) 
				     (relative-path? libpath)))
	(raise-type-error 'find-executable-path "#f or relative path or string" libpath))
      (letrec ([found-exec
		(lambda (exec-name)
                  (if libpath
		      (let-values ([(base name isdir?) (split-path exec-name)])
			(let ([next
			       (lambda ()
				 (let ([resolved (resolve-path exec-name)])
				   (cond
				    [(equal? resolved exec-name) #f]
				    [(relative-path? resolved)
				     (found-exec (build-path base resolved))]
				    [else (found-exec resolved)])))])
			  (or (and reverse? (next))
			      (if (path? base)
				  (let ([lib (build-path base libpath)])
				    (and (or (directory-exists? lib) 
					     (file-exists? lib))
					 lib))
				  #f)
			      (and (not reverse?) (next)))))
		      exec-name))])
	(if (and (relative-path? program)
		 (let-values ([(base name dir?) (split-path program)])
		   (eq? base 'relative)))
	    (let ([paths-str (getenv "PATH")]
		  [win-add (lambda (s) (if (eq? (system-type) 'windows) 
					   (cons (bytes->path #".") s) 
					   s))])
	      (let loop ([paths (if paths-str 
				    (win-add (path-list-string->path-list paths-str null))
				    null)])
		(if (null? paths)
		    #f
		    (let* ([base (path->complete-path (car paths))]
			   [name (build-path base program)])
		      (if (file-exists? name)
			  (found-exec name)
			  (loop (cdr paths)))))))
	    (let ([p (path->complete-path program)])
	      (and (file-exists? p) (found-exec p)))))]
     [(program libpath) (find-executable-path program libpath #f)]
     [(program) (find-executable-path program #f #f)]))

  ;; ------------------------------ Memtrace ------------------------------
  
  (define-syntax memory-trace-lambda
     (lambda (x)
       (syntax-case x ()
 	[(_ args body ...)
 	 (with-syntax ([contmark (datum->syntax-object x (gensym))]
 		       [func (datum->syntax-object x (gensym))]
 		       [newmark (datum->syntax-object x (gensym))])
 	   (syntax
	     (let ([contmark #f])
 	       (let ([func (lambda args
 			     (let ([newmark (unioned-memtrace-tracking-value
 					      contmark)])
 			       (with-continuation-mark
 				 memory-trace-continuation-mark newmark
 				 body ...)))])
 		 (set! contmark (new-memtrace-tracking-function func))
 		 func))))])))
  
  ;; ------------------------------ Collections ------------------------------

  (define (-check-relpath who s)
    (unless (path-string? s)
      (raise-type-error who "path or valid-path string" s))
    (unless (relative-path? s)
      (raise (make-exn:fail:contract
	      (string->immutable-string
	       (format "~a: invalid relative path: ~s" who s))
	      (current-continuation-marks)))))

  (define (-check-collection who collection collection-path)
    (-check-relpath who collection) 
    (for-each (lambda (p) (-check-relpath who p)) collection-path))
  
  (define (-find-col who collection collection-path)
    (let ([all-paths (current-library-collection-paths)])
      (let cloop ([paths all-paths])
	(if (null? paths)
	    (raise
	     (make-exn:fail:filesystem
	      (string->immutable-string
	       (format "~a: collection not found: ~s in any of: ~s" 
		       who (if (null? collection-path)
			       collection
			       (apply build-path collection collection-path))
		       all-paths))
	      (current-continuation-marks)))
	    (let ([dir (build-path (car paths) collection)])
	      (if (directory-exists? dir)
		  (let* ([cpath (apply build-path dir collection-path)])
		    (if (directory-exists? cpath)
			cpath
			;; sub-collection not here; try next instance
			;; of the top-level collection
			(cloop (cdr paths))))
		  (cloop (cdr paths))))))))

  (define dll-suffix (system-type 'so-suffix))

  (define _loader.so
    (path-replace-suffix
     (bytes->path #"_loader.ss")
     dll-suffix))

  (define current-load/use-compiled
    (make-parameter
     (let ([default-load/use-compiled
	     (let* ([resolve (lambda (s)
			       (if (complete-path? s)
				   s
				   (let ([d (current-load-relative-directory)])
				     (if d (path->complete-path s d) s))))]
		    [date-of (lambda (a modes)
			       (ormap
				(lambda (compiled-dir)
				  (let ([a (a compiled-dir)])
				    (let ([v (file-or-directory-modify-seconds a #f (lambda () #f))])
				      (and v (cons a v)))))
				modes))]
		    [date>=?
		     (lambda (modes a bm)
		       (and a
			    (let ([am (date-of a modes)])
			      (or (and (not bm) am) 
				  (and am bm (>= (cdr am) (cdr bm)) am)))))])
	       (lambda (path expect-module)
		 (unless (path-string? path)
		   (raise-type-error 'load/use-compiled "path or valid-path string" path))
		 (let*-values ([(path) (resolve path)]
			       [(base file dir?) (split-path path)]
			       [(base) (if (eq? base 'relative) 'same base)]
			       [(modes) (use-compiled-file-paths)])
		   (let* ([get-so (lambda (file rep-sfx?)
				    (lambda (compiled-dir)
				      (build-path base
						  compiled-dir
						  "native"
						  (system-library-subpath)
						  (if rep-sfx?
						      (path-replace-suffix
						       file
						       dll-suffix)
						      file))))]
			  [zo (lambda (compiled-dir)
				(build-path base
					    compiled-dir
					    (path-replace-suffix file #".zo")))]
			  [so (get-so file #t)]
			  [_loader-so (get-so _loader.so #f)]
			  [path-d (date-of (lambda (dir) path) modes)]
			  [with-dir (lambda (t) 
				      (parameterize ([current-load-relative-directory 
						      (if (path? base) 
							  base 
							  (current-directory))])
					(t)))])
		     (cond
		      [(let ([_loader-d (date>=? modes _loader-so path-d)])
			 (and _loader-d
			      (let ([getter (load-extension (car _loader-d))])
				(let-values ([(loader modname) (getter (string->symbol 
									(bytes->string/latin-1
									 (path->bytes
									  (path-replace-suffix file #"")))))])
				  (and loader
				       (begin
					 (when expect-module
					   (unless (eq? modname expect-module)
					     (raise
					      (make-exn:fail
					       (string->immutable-string
						(format "load-extension: expected module declaration for `~a', found ~a through loader: ~e"
							expect-module
							(if modname 
							    (format "module declaration for `~a'" modname)
							    "none")
							(car _loader-d)))
					       (current-continuation-marks)))))
					 loader))))))
		       => (lambda (loader) (with-dir loader))]
		      [(date>=? modes so path-d)
		       => (lambda (so-d)
			    (with-dir (lambda () ((current-load-extension) (car so-d) expect-module))))]
		      [(date>=? modes zo path-d)
		       => (lambda (zo-d)
			    (with-dir (lambda () ((current-load) (car zo-d) expect-module))))]
		      [else
		       (with-dir (lambda () ((current-load) path expect-module)))])))))])
       default-load/use-compiled)
     (lambda (p)
       (unless (and (procedure? p)
		    (procedure-arity-includes? p 2))
	 (raise-type-error 'current-load/use-compiled
			   "procedure (arity 2)"
			   p))
       p)))

  (define (collection-path collection . collection-path) 
    (-check-collection 'collection-path collection collection-path)
    (-find-col 'collection-path collection collection-path))

  (define (load/use-compiled f) ((current-load/use-compiled) f #f))

  (current-reader-guard (let ([default-reader-guard (lambda (path) path)])
			  default-reader-guard))

  (define -re:dir #rx#"(.+?)/+(.*)")
  (define -re:auto #rx#"^,")
  (define -re:ok-relpath #rx#"^[-a-zA-Z0-9_. ]+(/+[-a-zA-Z0-9_. ]+)*$")
  (define -module-hash-table-table (make-hash-table 'weak)) ; weak map from namespace to module ht
  (define -path-cache (make-hash-table 'weak 'equal)) ; weak map from `lib' path + corrent-library-paths to symbols
  
  (define -loading-filename (gensym))
  (define -loading-prompt-tag (make-continuation-prompt-tag 'module-loading))
  (define -prev-relto #f)
  (define -prev-relto-dir #f)

  (define (make-standard-module-name-resolver orig-namespace)
    (define planet-resolver #f)
    (define standard-module-name-resolver
      (case-lambda 
       [(s) 
	;; Just register s as loaded
	(when planet-resolver
	  ;; Let planet resolver register, too:
	  (planet-resolver s))
	(let ([ht (or (hash-table-get -module-hash-table-table
				      (namespace-module-registry (current-namespace))
				      #f)
		      (let ([ht (make-hash-table)])
			(hash-table-put! -module-hash-table-table
					 (namespace-module-registry (current-namespace))
					 ht)
			ht))])
	  (hash-table-put! ht s 'attach))]
       [(s relto stx) (standard-module-name-resolver s relto stx #t)]
       [(s relto stx load?)
	;; If stx is not #f, raise syntax error for ill-formed paths
	;; If s is #f, call to resolver is a notification from namespace-attach-module
	(cond
	 [(and (pair? s) (eq? (car s) 'planet))
	  (unless planet-resolver
	    (parameterize ([current-namespace orig-namespace])
	      (set! planet-resolver (dynamic-require '(lib "resolver.ss" "planet") 'planet-module-name-resolver))))
	  (planet-resolver s relto stx load?)]
	 [else
	  (let ([get-dir (lambda ()
			   (or (and relto
				    (if (eq? relto -prev-relto)
					-prev-relto-dir
					(let ([rts (string->bytes/latin-1 (symbol->string relto))])
					  (and (regexp-match? -re:auto rts)
					       (let-values ([(base n d?)
							     (split-path 
							      (bytes->path
							       (subbytes rts 1 (bytes-length rts))))])
						 (set! -prev-relto relto)
						 (set! -prev-relto-dir base)
						 base)))))
			       (current-load-relative-directory)
			       (current-directory)))])
	    (let ([s-parsed
		   ;; Non-string result represents an error
		   (cond
		    [(string? s)
		     (let* ([dir (get-dir)])
		       (or (hash-table-get -path-cache (cons s dir) #f)
			   (let ([s (string->bytes/utf-8 s)])
			     (if (regexp-match? -re:ok-relpath s)
				 ;; Parse Unix-style relative path string
				 (let loop ([path dir][s s])
				   (let ([prefix (regexp-match -re:dir s)])
				     (if prefix
					 (loop (build-path path 
							   (let ([p (cadr prefix)])
							     (cond
							      [(bytes=? p #".") 'same]
							      [(bytes=? p #"..") 'up]
							      [else (bytes->path p)])))
					       (caddr prefix))
					 (build-path path (bytes->path s)))))
				 (list
				  (string-append
				   " (relative string form must contain only a-z, A-Z, 0-9, -, _, ., /, and "
				   "space, with no leading or trailing /)"))))))]
		    [(path? s) 
		     (if (absolute-path? s)
			 s
			 (list " (a path must be absolute)"))]
		    [(or (not (pair? s))
			 (not (list? s)))
		     #f]
		    [(eq? (car s) 'lib)
		     (or (hash-table-get -path-cache
					 (cons s (current-library-collection-paths))
					 #f)
			 (let ([cols (let ([len (length s)])
				       (if (= len 2)
					   (list "mzlib")
					   (if (> len 2)
					       (cddr s)
					       #f)))])
			   (and cols
				(andmap (lambda (x) (and (string? x) 
							 (relative-path? x))) cols)
				(string? (cadr s))
				(relative-path? (cadr s))
				(let ([p (-find-col 'standard-module-name-resolver (car cols) (cdr cols))])
				  (build-path p (cadr s))))))]
		    [(eq? (car s) 'file)
		     (and (= (length s) 2)
			  (let ([p (cadr s)])
			    (and (string? p)
				 (path-string? p)
				 (path->complete-path p (get-dir)))))]
		    [else #f])])
	      (unless (or (path? s-parsed)			  
			  (vector? s-parsed))
		(if stx
		    (raise-syntax-error
		     'require
		     (format "bad module path~a" (if s-parsed
						     (car s-parsed)
						     ""))
		     stx)
		    (raise-type-error 
		     'standard-module-name-resolver
		     (format "module path~a" (if s-parsed
						 (car s-parsed)
						 ""))
		     s)))
	      ;; At this point, s-parsed is a complete path
	      (let* ([filename (if (vector? s-parsed)
				   (vector-ref s-parsed 0)
				   (simplify-path (expand-path s-parsed)))]
		     [normal-filename (if (vector? s-parsed)
					  (vector-ref s-parsed 1)
					  (normal-case-path filename))])
		(let-values ([(base name dir?) (if (vector? s-parsed)
						   (values 'ignored (vector-ref s-parsed 2) 'ignored)
						   (split-path filename))])
		  (let* ([no-sfx (if (vector? s-parsed)
				     (vector-ref s-parsed 3)
				     (path-replace-suffix name #""))]
			 [abase (if (vector? s-parsed)
				    (vector-ref s-parsed 4)
				    (format ",~a" (bytes->string/latin-1 (path->bytes (normal-case-path base)))))])
		    (let ([modname (if (vector? s-parsed)
				       (vector-ref s-parsed 5)
				       (string->symbol (string-append
							abase
							(bytes->string/latin-1
							 (path->bytes no-sfx)))))]
			  [suffix (if (vector? s-parsed)
				      (vector-ref s-parsed 6)
				      (let ([m (regexp-match -re:suffix (path->bytes name))])
					(if m (car m) #t)))]
			  [ht (or (hash-table-get -module-hash-table-table
						  (namespace-module-registry (current-namespace))
						  #f)
				  (let ([ht (make-hash-table)])
				    (hash-table-put! -module-hash-table-table
						     (namespace-module-registry (current-namespace))
						     ht)
				    ht))])
		      ;; Loaded already?
		      (when load?
			(let ([got (hash-table-get ht modname #f)])
			  (when got
			    ;; Check the suffix, which gets lost when creating a key:
			    (unless (or (symbol? got) (equal? suffix got))
			      (error
			       'standard-module-name-resolver
			       "module previously loaded with suffix ~s, cannot load with suffix ~s: ~e"
			       (if (eq? #t got) "" got)
			       (if (eq? #t suffix) "" suffix)
			       filename)))
			  (unless got
			    ;; Currently loading?
			    (let ([l (let ([tag (if (continuation-prompt-available? -loading-prompt-tag)
                                                    -loading-prompt-tag
                                                    (default-continuation-prompt-tag))])
                                       (continuation-mark-set->list
                                        (current-continuation-marks tag)
                                        -loading-filename
                                        tag))]
				  [ns (current-namespace)])
			      (for-each
			       (lambda (s)
				 (when (and (equal? (cdr s) normal-filename)
					    (eq? (car s) ns))
				   (error
				    'standard-module-name-resolver
				    "cycle in loading at ~e: ~e"
				    filename
				    (map cdr (reverse (cons s l))))))
			       l))
			    (let ([prefix (string->symbol abase)])
                              ((if (continuation-prompt-available? -loading-prompt-tag)
                                   (lambda (f) (f))
                                   (lambda (f) (call-with-continuation-prompt f -loading-prompt-tag)))
                               (lambda ()
                                 (with-continuation-mark -loading-filename (cons (current-namespace) normal-filename)
                                   (parameterize ([current-module-name-prefix prefix])
                                     ((current-load/use-compiled) 
                                      filename 
                                      (string->symbol (bytes->string/latin-1 (path->bytes no-sfx))))))))
                              (hash-table-put! ht modname suffix)))))
		      ;; If a `lib' path, cache pathname manipulations
		      (when (and (not (vector? s-parsed))
				 (or (string? s)
				     (and (pair? s)
					  (eq? (car s) 'lib))))
			(hash-table-put! -path-cache
					 (if (string? s)
					     (cons s (get-dir))
					     (cons s (current-library-collection-paths)))
					 (vector filename
						 normal-filename
						 name
						 no-sfx
						 abase
						 modname
						 suffix)))
		      ;; Result is the module name:
		      modname))))))])]))
    standard-module-name-resolver)
    
  (define find-library-collection-paths
    (case-lambda
     [() (find-library-collection-paths null)]
     [(extra-collects-dirs)
      (let ([user-too? (use-user-specific-search-paths)]
	    [cons-if (lambda (f r) (if f (cons f r) r))])
	(path-list-string->path-list
	 (if user-too?
	     (or (getenv "PLTCOLLECTS") "")
	     "")
	 (cons-if
	  (and user-too?
	       (build-path (find-system-path 'addon-dir)
			   (version)
			   "collects"))
	  (let loop ([l (append
			 extra-collects-dirs
			 (list (find-system-path 'collects-dir)))])
	    (if (null? l)
		null
		(let* ([collects-path (car l)]
		       [v
			(cond
			 [(complete-path? collects-path) collects-path]
			 [(absolute-path? collects-path)
			  (path->complete-path collects-path
					       (find-executable-path (find-system-path 'exec-file) #f #t))]
			 [else
			  (find-executable-path (find-system-path 'exec-file) collects-path #t)])])
		  (if v
		      (cons (simplify-path (path->complete-path v (current-directory)))
			    (loop (cdr l)))
		      (loop (cdr l)))))))))]))

  ;; -------------------------------------------------------------------------

  (define (port? x) (or (input-port? x) (output-port? x)))

  (define-values (struct:guard make-guard guard? guard-ref guard-set!)
    (make-struct-type 'evt #f 1 0 #f (list (cons prop:evt 0)) (current-inspector) #f '(0)))

  (define (guard-evt proc)
    (unless (and (procedure? proc)
		 (procedure-arity-includes? proc 0))
      (raise-type-error 'guard-evt "procedure (arity 0)" proc))
    (make-guard (lambda (self) (proc))))

  (define (channel-get ch)
    (unless (channel? ch)
      (raise-type-error 'channel-get "channel" ch))
    (sync ch))

  (define (channel-try-get ch)
    (unless (channel? ch)
      (raise-type-error 'channel-try-get "channel" ch))
    (sync/timeout 0 ch))

  (define (channel-put ch val)
    (unless (channel? ch)
      (raise-type-error 'channel-put "channel" ch))
    (and (sync (channel-put-evt ch val)) (void)))

  ;; -------------------------------------------------------------------------

  (define interaction-environment (lambda () (current-namespace)))

  (define (scheme-report-environment n)
    (unless (= n 5)
      (raise-type-error 'scheme-report-environment "5" n))
    (mk-r5rs #f))

  (define (null-environment n)
    (unless (= n 5)
      (raise-type-error 'null-environment "5" n))
    (mk-r5rs #t))

  (define (mk-r5rs stx-only?)
    (let ([n (make-namespace 'empty)]
	  [orig (make-namespace)])
      (parameterize ([current-namespace n])
	(namespace-attach-module orig '#%r5rs)
	(namespace-require '#%r5rs)
	(namespace-transformer-require '(only mzscheme syntax-rules))
	(unless stx-only?
	  (for-each
	   (lambda (n)
	     (namespace-set-variable-value! n (dynamic-require 'mzscheme n)))
	   '(car 
	     cdr caar cadr cdar cddr
	     caaar caadr cadar caddr cdaar cdadr cddar cdddr
	     caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	     cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	     map = < > <= >= max min + - * / 
	     abs gcd lcm exp log sin cos tan not eq?
	     call-with-current-continuation make-string
	     symbol->string string->symbol make-rectangular 
	     exact->inexact inexact->exact number->string string->number 
	     rationalize output-port? current-input-port current-output-port current-error-port 
	     open-input-file open-output-file close-input-port close-output-port
	     with-output-to-file transcript-on transcript-off flush-output
	     string-length string-ci<=? string-ci>=? string-append 
	     string->list list->string string-fill! 
	     vector-length vector->list list->vector vector-fill!
	     char-alphabetic? char-numeric? char-whitespace? 
	     char-upper-case? char-lower-case? char->integer integer->char char-downcase
	     call-with-output-file call-with-input-file with-input-from-file
	     apply for-each symbol? pair? cons set-car! set-cdr! null? list? list length append reverse
	     list-tail list-ref memq memv member assq assv assoc procedure?
	     number? complex? real? rational? integer? exact? inexact? zero?
	     positive?  negative? odd? even? 
	     quotient remainder modulo floor ceiling truncate round 
	     numerator denominator asin acos atan sqrt
	     expt make-polar real-part imag-part angle magnitude input-port?
	     read read-char peek-char eof-object?
	     char-ready? write display newline write-char load 
	     string? string string-ref string-set! string=? substring string-copy
	     string-ci=? string<? string>? string<=? string>=? string-ci<? string-ci>?
	     vector? make-vector vector vector-ref vector-set! 
	     char? char=? char<? char>? char<=? char>=? 
	     char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=? 
	     char-upcase boolean? eqv? equal? force
	     call-with-values values eval port? scheme-report-environment null-environment 
	     interaction-environment dynamic-wind))))
      n))

  ;; -------------------------------------------------------------------------

  (provide rationalize 
	   path-string? path-replace-suffix normal-case-path
	   read-eval-print-loop
	   load/cd memory-trace-lambda
	   load-relative load-relative-extension
	   path-list-string->path-list find-executable-path
	   collection-path load/use-compiled current-load/use-compiled
	   port? guard-evt
	   channel-get channel-try-get channel-put
	   find-library-collection-paths
	   interaction-environment scheme-report-environment null-environment
	   make-standard-module-name-resolver))

;;----------------------------------------------------------------------
;; #%stxmz-body

(module #%stxmz-body #%kernel
  (require #%stxcase-scheme #%define)
  (require-for-syntax #%kernel #%stx)

  ;; So that expansions print the way the MzScheme programmer expects:
  (require (rename #%kernel #%plain-module-begin #%module-begin))

  (define-syntax mzscheme-in-stx-module-begin
    (lambda (stx)
      (if (stx-pair? stx)
	  (datum->syntax-object
	   (quote-syntax here)
	   (list* (quote-syntax #%plain-module-begin)
                  (datum->syntax-object
		   stx
                   (list (quote-syntax require-for-syntax) 'mzscheme))
		  (stx-cdr stx))
	   stx)
	  (raise-syntax-error #f "bad syntax" stx))))

  (define-syntax #%top-interaction
    (lambda (stx)
      (if (eq? 'top-level (syntax-local-context))
          'ok
          (raise-syntax-error
           #f
           "not at top level"
           stx))
      (datum->syntax-object stx (stx-cdr stx) stx stx)))

  (provide mzscheme-in-stx-module-begin
           #%top-interaction))

;;----------------------------------------------------------------------
;; mzscheme: provide everything

(module mzscheme #%kernel
  (require #%more-scheme)
  (require #%misc)
  (require #%stxcase-scheme)
  (require #%stx)
  (require #%stxmz-body)
  (require #%qqstx)
  (require #%define)
  (require #%expobs) ; so it's attached
  (require (only #%foreign))  ; so it's attached, but doesn't depend on any exports

  (provide (all-from #%more-scheme)
	   (all-from-except #%misc make-standard-module-name-resolver)
	   (all-from-except #%stxcase-scheme -define -define-syntax)
	   identifier? ;; from #%stx
	   (all-from #%qqstx)
	   (all-from #%define)
	   (all-from-except #%kernel #%module-begin)
           #%top-interaction
	   (rename mzscheme-in-stx-module-begin #%module-begin)
	   (rename #%module-begin #%plain-module-begin)))

;;----------------------------------------------------------------------
;; r5rs syntax (used by null-environment and scheme-report-environment)

(module #%r5rs mzscheme
  
  ;; Copied from R5rS, but with an added `let' around body
  (define undefined (letrec ([u u]) u))
  (define-syntax r5rs:letrec
    (syntax-rules ()
      ((r5rs:letrec ((var1 init1) ...) body ...)
       (r5rs:letrec "generate_temp_names"
	 (var1 ...)
	 ()
	 ((var1 init1) ...)
	 body ...))
      ((r5rs:letrec "generate_temp_names"
	 ()
	 (temp1 ...)
	 ((var1 init1) ...)
	 body ...)
       (let ((var1 undefined) ...)
	 (let ((temp1 init1) ...)
	   (set! var1 temp1)
	   ...
	   (let ()
	     body ...))))
      ((r5rs:letrec "generate_temp_names"
	 (x y ...)
	 (temp ...)
	 ((var1 init1) ...)
	 body ...)
       (r5rs:letrec "generate_temp_names"
	 (y ...)
	 (newtemp temp ...)
	 ((var1 init1) ...)
	 body ...))))

  (provide quasiquote unquote unquote-splicing 
	   if let and or cond case define delay do
	   (rename r5rs:letrec letrec)
	   let* begin lambda quote set!
	   define-syntax let-syntax letrec-syntax

	   ;; We have to include the following MzScheme-isms to do anything,
	   ;; but they're not legal R5RS names, anyway.
	   #%app #%datum #%top #%top-interaction))

;;----------------------------------------------------------------------
;; init namespace

(require (only mzscheme namespace-require/copy))

(begin
  (namespace-require/copy 'mzscheme)
  (require-for-syntax mzscheme))

(current-module-name-resolver 
 ((dynamic-require '#%misc 'make-standard-module-name-resolver) (current-namespace)))

