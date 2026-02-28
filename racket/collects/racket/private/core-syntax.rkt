(module core-syntax '#%kernel

  ; ======================================================================
  ; Many of the base syntactic forms of Racket-- things like `and`, `let`,
  ; and `cond`. These are written together in a single module for startup
  ; time reasons, and this module forms a nice base for defining the rest
  ; of `#lang racket/base`.
  ; ======================================================================
  ;

  (#%require (for-syntax "stx.rkt" '#%kernel))

  (#%provide let let* letrec
             let*-values  ; {let,letrec}-values are kernel
             let-syntax letrec-syntax
             let-syntaxes letrec-syntaxes
             quasiquote
             and or)

  ; --------------------------------------------------
  ;
  ;
  ;   ;;;                                ;;;;
  ;     ;               ;               ;
  ;     ;               ;               ;
  ;     ;       ;;;     ;               ;       ;;     ; ;;; ; ;; ;;   ;;;;
  ;     ;      ;   ;  ;;;;;;          ;;;;;;   ;  ;    ;;  ; ;; ;; ;  ;    ;
  ;     ;     ;    ;    ;               ;     ;    ;   ;   ; ;  ;  ;  ;
  ;     ;     ;;;;;;    ;               ;     ;    ;   ;     ;  ;  ;   ;;
  ;     ;     ;         ;               ;     ;    ;   ;     ;  ;  ;     ;;
  ;     ;     ;         ;               ;     ;    ;   ;     ;  ;  ;       ;
  ;     ;      ;        ;               ;      ;  ;    ;     ;  ;  ;  ;    ;
  ;     ;;;     ;;;;     ;;;            ;       ;;     ;     ;  ;  ;   ;;;;
  ;
  ;
  ; let, let*, letrec, let-values, let-syntax, ...
  ;

  (begin-for-syntax
    (define-values (raise-if-duplicate-ids)
      (lambda (ids context-stx)
        (define-values (dup) (stx-first-duplicate-id ids))
        (raise-syntax-error-if dup "duplicate identifier" context-stx dup)))

    ; ((parse-and-validate-letlike-clause* context-stx multi? allow-crossclause-dups?) clause-stx)
    ;   context-stx              : syntax?   full form, for error reporting
    ;   multi?                   : boolean?  #t if bindings should be lists of ids, #f if a single id
    ;   allow-crossclause-dups?  : boolean?  if #t, check bindings within any single clause are distinct
    ;                                        if #f, do nothing; caller responsible for checking all clauses together
    ;   clause-stx               : syntax?   if multi?, then expected #'[(id ...) bound-expr]
    ;                                        else expected #'[id bound-expr]
    ;  -> (list/c (listof identifier?) syntax?)
    (define-values (parse-and-validate-letlike-clause*)
      (lambda (context-stx multi? allow-crossclause-dups?)
        (lambda (clause-stx)
          (define-values (clause-lst) (syntax->list clause-stx))
          (raise-syntax-error-unless (if clause-lst
                                         (= 2 (length clause-lst))
                                         #f)
                                     (if multi?
                                         "bad syntax (not an identifier list and expression for bindings)"
                                         "bad syntax (not an identifier and expression for a binding)")
                                     context-stx
                                     clause-stx)
          (define-values (ids) (if multi?
                                   (syntax->list (car clause-lst))
                                   (list (car clause-lst))))
          (if multi?
              (raise-syntax-error-unless ids
                                         "bad syntax (not a list of identifiers)"
                                         context-stx
                                         (car clause-lst))
              (void))
          (for-each (lambda (id)
                      (raise-syntax-error-unless (identifier? id)
                                                 "bad syntax (not an identifier)"
                                                 context-stx
                                                 id))
                    ids)
          (if allow-crossclause-dups?
              (raise-if-duplicate-ids ids context-stx)
              (void))
          (list ids (cadr clause-lst)))))
  
    ; (parse-and-validate-letlike stx allow-named? multi? allow-crossclause-dups?)
    ;    stx of the form #'(letlike [maybe-name] ([binding/bindings bound-expr] ...) body ...+)
    ;    allow-named?           : boolean?    #t for just let
    ;    multi?                 : boolean?    #t for {let,let*,letrec}-{values,syntaxes}
    ;    allow-crossclause-dups : boolean?    #t for let*{,-syntaxes,-values}
    ;  -> (values
    ;       (if/c allow-named? (or/c identifier? #f) #f)     name, if was named let
    ;       (listof (list/c (listof identifier?) syntax?))   binding clauses, normalized and dup-checked
    ;       (listof syntax?))                                nonempty listof body clauses
    (define-values (parse-and-validate-letlike)
      (lambda (stx allow-named? multi? allow-crossclause-dups?)
        (raise-syntax-error-unless (stx-list? stx)
                                   "bad syntax (illegal use of `.')"
                                   stx)
        (let-values ([(me) (stx-car stx)]
                     [(tail) (stx-cdr stx)])
          (raise-syntax-error-unless (stx-pair? tail)
                                     (if allow-named?
                                         "bad syntax (missing name or binding pairs)"
                                         "bad syntax (missing binding pairs)")
                                     stx)
          (let-values ([(maybe-name tail) (if (if allow-named? (identifier? (stx-car tail)) #f)
                                              (values (stx-car tail) (stx-cdr tail))
                                              (values #f tail))])
            (raise-syntax-error-unless (stx-pair? tail)  ; redundant check when not named; this is fine
                                       "bad syntax (missing binding pairs)"
                                       stx)
            (let-values ([(clauses-stx) (stx-car tail)]
                         [(body-stxl) (stx-cdr tail)])
              (define-values (clause-stxs) (syntax->list clauses-stx))
              (raise-syntax-error-unless clause-stxs
                                         (if multi?
                                             "bad syntax (not a sequence of identifier-list--expression bindings)"
                                             "bad syntax (not a sequence of identifier--expression bindings)")
                                         stx
                                         clauses-stx)
              (define-values (bindings)
                (map (parse-and-validate-letlike-clause* stx multi? allow-crossclause-dups?)
                     clause-stxs))
              (if allow-crossclause-dups?
                  (void)
                  (raise-if-duplicate-ids (apply append (map car bindings))
                                          stx))
              (raise-syntax-error-unless (stx-pair? body-stxl)
                                         "bad syntax (missing body)"
                                         stx)
              (values maybe-name
                      bindings
                      body-stxl))))))

    ; (gen-named-let name initial-bindings body)
    ;   name             : identifier?
    ;   initial-bindings : (listof (list/c (list/c identifier?) syntax?))
    ;   body             : syntax?
    ;  -> any/c
    ; Generate the syntax (suitable to be `datum->syntax`'d) for a named let.
    ; This function assumes that each clause in the binding list has only a
    ; single identifier; hence `(list/c identifier?)` in the contract instead
    ; of a `listof`. This is ensured by `parse-and-validate-letlike`.
    (define-values (gen-named-let)
      (lambda (name initial-bindings body)
        (list* (quote-syntax #%app)
               (list (quote-syntax letrec-values)
                     (list (list (list name)
                                 (list* (quote-syntax lambda)
                                        (map caar initial-bindings)
                                        body)))
                     name)
               (map cadr initial-bindings))))

    ; (gen-boring-let target bindings body)
    ;   target   : (or/c #'let-values #'letrec-values)
    ;   bindings : (listof (list/c (listof identifier?) syntax?))
    ;   body     : syntax?
    ;  -> any/c
    ; Generate the syntax (suitable to be `datum->syntax`'d) for a standard
    ; value let (either let-values or letrec-values, which are the two kernel
    ; forms for value bindings).
    (define-values (gen-boring-let)
      (lambda (target bindings body)
        (list* target bindings body)))

    ; (gen-boring-let target bindings body)
    ;   target   : #'let-values
    ;   bindings : (listof (list/c (listof identifier?) syntax?))
    ;   body     : syntax?
    ;  -> any/c
    ; Generate the syntax (suitable to be `datum->syntax`'d) for a `let*`
    ; or a `let*-values`. The only value which makes sense for target is
    ; #'let-values.
    (define-values (gen-star-let)
      (lambda (target bindings body)
        (if (null? bindings)
            (list* target null body)
            (letrec-values ([(gen)  ; -> (listof body-expression)
                             (lambda (bindings)
                               (if (null? bindings)
                                   body
                                   (list (list* target
                                                (list (car bindings))
                                                (gen (cdr bindings))))))])
              (car (gen bindings))))))

    ; (gen-syntaxes-let target bindings body)
    ;   target   : ignored
    ;   bindings : (listof (list/c (listof identifier?) syntax?))
    ;   body     : syntax?
    ;  -> any/c
    ;
    ; Generate the syntax to implement a `let-syntax` or `let-syntaxes`. The
    ; expansion is
    ;
    ;      (let-syntax ([id bound-expr] ...) body ...)
    ;      (let-syntaxes ([(id ...) bound-expr] ...) body ...)
    ;
    ; to
    ;
    ;    (letrec-syntaxes+values
    ;        ([(id/tmp ...) bound-expr] ...)
    ;        ()
    ;      (letrec-syntaxes+values
    ;         ([(id ...) (values (make-rename-transformer (quote-syntax id/tmp)) ...)] ...)
    ;         ()
    ;       body ...))
    ;
    ; This is necessary because the only kernel form for local syntax bindings is
    ; letrec-syntaxes+values, and the rename transformers make the scope work right.
    ; For the `id/tmp`, we use a temporary with the same spelling and location as the
    ; original, but with a fresh scope. (Note that avoiding ambiguous bindings requires
    ; stripping the old scopes from the ids, which in turn requires using a fresh scope
    ; for each individual id.)
    (define-values (gen-syntaxes-let)
      (lambda (_target bindings body)
        (define-values (tmp-bindings)
          (map (lambda (binding)
                 (list (map (lambda (id)
                              ((make-syntax-introducer) (datum->syntax #f (syntax-e id) id)))
                            (car binding))
                       (cadr binding)))
               bindings))
        (define-values (rename-bindings)
          (map (lambda (binding tmp-binding)
                 (list (car binding)
                       (list* (quote-syntax #%app)
                              (quote-syntax values)
                              (map (lambda (tmp-id)
                                     (list (quote-syntax #%app)
                                           (quote-syntax make-rename-transformer)
                                           (list (quote-syntax quote-syntax) tmp-id)))
                                   (car tmp-binding)))))
               bindings
               tmp-bindings))
        (list (quote-syntax letrec-syntaxes+values)
              tmp-bindings
              null
              (list* (quote-syntax letrec-syntaxes+values)
                     rename-bindings
                     null
                     body))))

    ; (gen-recsyntaxes-let target bindings body)
    ;   target   : ignored
    ;   bindings : (listof (list/c (listof identifier?) syntax?))
    ;   body     : syntax?
    ;  -> any/c
    (define-values (gen-recsyntaxes-let)
      (lambda (_target bindings body)
        (list* (quote-syntax letrec-syntaxes+values)
               bindings
               null
               body)))

    ; (make-let-transformer allow-named? star? multi? gen target)
    ;   allow-named? : boolean?
    ;   star?        : (if/c allow-named? #f boolean?)
    ;   multi?       : (if/c allow-named? #f boolean?)
    ;   gen          : one of the gen-something-let functions above
    ;   target       : value suitable for `gen`'s first argument
    ;  -> (-> syntax? syntax?)
    ; Makes a transformer for handling a let-like form where the form's syntax
    ; is determined by `allow-named?`, `star?`, and `multi?` and the output is
    ; determined by `gen` and `target`. Note that `allow-named?` should be true
    ; only for `let` and the transformer assumes this.
    (define-values (make-let-transformer)
      (lambda (allow-named? star? multi? gen target)
        (lambda (stx)
          (define-values (name bindings body)
            (parse-and-validate-letlike stx allow-named? multi? star?))
          (datum->syntax #f
                         (if name
                             (gen-named-let name bindings body)
                             (gen target bindings body))
                         stx
                         stx)))))

  (define-syntaxes (let)               (make-let-transformer #t #f #f gen-boring-let (quote-syntax let-values)))
  (define-syntaxes (let*)              (make-let-transformer #f #t #f gen-star-let (quote-syntax let-values)))
  (define-syntaxes (letrec)            (make-let-transformer #f #f #f gen-boring-let (quote-syntax letrec-values)))

  (define-syntaxes (let*-values)       (make-let-transformer #f #t #t gen-star-let (quote-syntax let-values)))

  (define-syntaxes (let-syntax)        (make-let-transformer #f #f #f gen-syntaxes-let 'ignored))
  (define-syntaxes (letrec-syntax)     (make-let-transformer #f #f #f gen-recsyntaxes-let 'ignored))

  (define-syntaxes (let-syntaxes)      (make-let-transformer #f #f #t gen-syntaxes-let 'ignored))
  (define-syntaxes (letrec-syntaxes)   (make-let-transformer #f #f #t gen-recsyntaxes-let 'ignored))

  ; --------------------------------------------------
  ;
  ;
  ;     ;;;;    ;;;;
  ;    ;   ;   ;   ;
  ;   ;    ;  ;    ;
  ;   ;    ;  ;    ;
  ;   ;    ;  ;    ;
  ;   ;    ;  ;    ;
  ;   ;   ;;  ;   ;;
  ;    ;;; ;   ;;; ;
  ;        ;       ;
  ;        ;       ;
  ;        ;       ;
  ;
  ;
  ; quasiquote
  ;

  (define-values (qq-append)
    (lambda (a b)
      (if (list? a)
	  (append a b)
	  (raise-argument-error 'unquote-splicing "list?" a))))

  (define-syntaxes (quasiquote)
    (let-values ([(here) (quote-syntax here)] ; id with module bindings, but not lexical
                 [(unquote-stx) (quote-syntax unquote)]
                 [(unquote-splicing-stx) (quote-syntax unquote-splicing)])
      (lambda (in-form)
	(if (identifier? in-form)
	    (raise-syntax-error #f "bad syntax" in-form)
            (void))
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
			    (if (free-identifier=? (quote-syntax list) (car d))
				#t
				(free-identifier=? (quote-syntax list*) (car d)))
			    #f)
			(list* (car d) a (cdr d))
			(list (quote-syntax list*) a d))))))
	  (datum->syntax
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
					(free-identifier=? first unquote-stx)
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
				       x)
                                      (void))
				  (if (zero? level)
				      (stx-car rest)
				      (qq-list x (sub1 level))))
				(if (if (if (identifier? first)
					    (free-identifier=? first (quote-syntax quasiquote))
					    #f)
					(stx-list? x)
					#f)
				    (qq-list x (add1 level))
				    (if (if (if (identifier? first)
						(free-identifier=? first unquote-splicing-stx)
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
						    (if (free-identifier=? (stx-car first)
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
						   x)
                                                  (void))
					      (let-values
						  (((uqsd) (stx-car rest))
						   ((old-l) (stx-cdr x))
						   ((l) (qq (stx-cdr x) level)))
						(if (zero? level)
						    (let-values
							(((l) (normal l old-l)))
                                                      (if (stx-null? l)
                                                          uqsd
                                                          (list (quote-syntax qq-append)
                                                                uqsd l)))
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
                                ;; special case: disallow #(unquote <e>)
                                (if (stx-pair? l)
                                    (let-values ([(first) (stx-car l)])
                                      (if (identifier? first)
                                          (if (free-identifier=? first unquote-stx)
                                              (raise-syntax-error
                                               'unquote
                                               "invalid context within quasiquote"
                                               in-form
                                               first)
                                              (void))
                                          (void)))
                                    (void))
				(let-values
				    (((l2) (qq l level)))
                                  (if (eq? l l2)
                                      x
                                      (list (quote-syntax list->vector) l2))))
			      (if (if (syntax? x) (box? (syntax-e x)) #f)
				  (let-values
				      (((v) (unbox (syntax-e x))))
				    (let-values
					(((qv) (qq v level)))
				      (if (eq? v qv)
                                          x
                                          (list (quote-syntax box) qv))))
                                  (if (if (syntax? x) 
                                          (if (struct? (syntax-e x)) 
                                              (prefab-struct-key (syntax-e x))
                                              #f)
                                          #f)
                                      ;; pre-fab struct
                                      (let-values
                                          (((l) (cdr (vector->list (struct->vector (syntax-e x))))))
                                        (let-values
                                            (((l2) (qq l level)))
                                          (if (eq? l l2)
                                              x
                                              (list (quote-syntax apply)
                                                    (quote-syntax make-prefab-struct)
                                                    (list (quote-syntax quote)
                                                          (prefab-struct-key (syntax-e x)))
                                                    l2))))
                                      ;; hash[eq[v]]
                                      (if (if (syntax? x)
                                              (hash? (syntax-e x))
                                              #f)
                                          (letrec-values
                                              (((qq-hash-assocs)
						(lambda (x level)
						  (if (null? x)
						      x
						      (let-values
						          (((pair) (car x)))
                                                        (let-values ([(val)
                                                                      (qq (datum->syntax here (cdr pair)) level)]
                                                                     [(rest)
                                                                      (qq-hash-assocs (cdr x) level)])
                                                          (if (if (eq? val (cdr pair))
                                                                  (eq? rest (cdr x))
                                                                  #f)
                                                              x
                                                              (apply-cons
                                                               (list (quote-syntax list*)
                                                                     (list (quote-syntax quote)
                                                                           (datum->syntax here (car pair)))
                                                                     (if (eq? val (cdr pair))
                                                                         (list (quote-syntax quote)
                                                                               val)
                                                                         val))
                                                               (if (eq? rest (cdr x))
                                                                   (list (quote-syntax quote)
                                                                         rest)
                                                                   rest)))))))))
                                            (let-values (((l0) (hash-map (syntax-e x) cons #t)))
                                              (let-values
                                                  (((l) (qq-hash-assocs l0 level)))
                                                (if (eq? l0 l)
                                                    x
                                                    (list (if (hash-eq? (syntax-e x))
                                                              (quote-syntax make-immutable-hasheq)
                                                              (if (hash-eqv? (syntax-e x))
                                                                  (quote-syntax make-immutable-hasheqv)
                                                                  (if (hash-equal-always? (syntax-e x))
                                                                      (quote-syntax make-immutable-hashalw)
                                                                      (quote-syntax make-immutable-hash))))
                                                          l)))))
                                          x)))))))))
	      (qq form 0))
	    form)
	   in-form)))))

  ; --------------------------------------------------
  ;
  ;
  ;                        ;       ;
  ;                        ;       ;
  ;                        ;      ;
  ;     ;;;;  ; ;;;     ;;;;      ;     ;;     ; ;;;
  ;    ;   ;  ;;   ;   ;   ;     ;     ;  ;    ;;  ;
  ;   ;    ;  ;    ;  ;    ;    ;     ;    ;   ;   ;
  ;   ;    ;  ;    ;  ;    ;    ;     ;    ;   ;
  ;   ;    ;  ;    ;  ;    ;   ;      ;    ;   ;
  ;   ;    ;  ;    ;  ;    ;   ;      ;    ;   ;
  ;   ;   ;;  ;    ;  ;   ;;  ;        ;  ;    ;
  ;    ;;; ;  ;    ;   ;;; ;  ;         ;;     ;
  ;
  ;
  ; `and` and `or`
  ;

  (define-syntaxes (and)
    (let-values ([(here) (quote-syntax here)])
      (lambda (x)
	(if (not (stx-list? x))
	    (raise-syntax-error #f "bad syntax" x)
            (void))
	(let-values ([(e) (stx-cdr x)])
	  (if (stx-null? e)
	      (quote-syntax #t)
	      (if (if (stx-pair? e)
		      (stx-null? (stx-cdr e))
		      #t)
                  (datum->syntax
                   here
                   (list (quote-syntax #%expression)
                         (stx-car e))
                   x)
		  (datum->syntax
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
	    (raise-syntax-error #f "bad syntax" x)
            (void))
	(let-values ([(e) (stx-cdr x)])
	  (if (stx-null? e) 
	      (quote-syntax #f)
	      (if (if (stx-pair? e)
		      (stx-null? (stx-cdr e))
		      #f)
                  (datum->syntax
                   here
                   (list (quote-syntax #%expression)
                         (stx-car e))
                   x)
		  (if (stx-list? e)
		      (let-values ([(tmp) 'or-part])
			(datum->syntax
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

  ;
  ; --------------------------------------------------
  )
