;; constant construction code generator
;; (c) 1996-7 Sebastian Good
;; (c) 1997-8 PLT, Rice University

; Handles code-generation for constructing constants.

; Symbols and floating point numbers are handled specially,
;  in a way that allows the generated C code to be both 
;  efficient and small. 
; Other kinds of constants are constrcted by generating code
;  that is prefixed onto the beginning of the program.

(module const mzscheme
  (require (lib "unit.ss")
	   (lib "list.ss")
	   (lib "etc.ss"))

  (require (lib "zodiac-sig.ss" "syntax")
	   (lib "stx.ss" "syntax"))

  (require "sig.ss")
  (require "../sig.ss")

  (provide const@)
  (define-unit const@
      (import (prefix compiler:option: compiler:option^)
	      compiler:library^
	      compiler:cstructs^
	      (prefix zodiac: zodiac^)
	      compiler:analyze^
	      compiler:zlayer^
	      compiler:vmstructs^
	      compiler:top-level^
	      compiler:driver^)
      (export compiler:const^)
      
      (define const:symbol-table (make-hash-table))
      (define const:symbol-counter 0)
      (define const:inexact-table (make-hash-table))
      (define const:inexact-counter 0)
      (define const:number-table (make-hash-table))
      (define const:string-table (make-hash-table))
      (define const:bytes-table (make-hash-table))
      (define const:string-counter 0)

      (define (const:get-symbol-table) const:symbol-table)
      (define (const:get-symbol-counter) const:symbol-counter)
      (define (const:get-inexact-table) const:inexact-table)
      (define (const:get-inexact-counter) const:inexact-counter)
      (define (const:get-string-table) const:string-table)
      (define (const:get-bytes-table) const:bytes-table)

      (define vector-table (make-hash-table))

      (define compiler:static-list null)
      (define compiler:per-load-static-list null)

      (define (compiler:get-static-list) compiler:static-list)
      (define (compiler:get-per-load-static-list) compiler:per-load-static-list)

      (define new-uninterned-symbols null) ; list of (cons sym pos)

      (define (const:init-tables!)
	(set! const:symbol-table (make-hash-table))
	(set! const:symbol-counter 0)
	(set! const:inexact-table (make-hash-table))
	(set! const:inexact-counter 0)
	(set! const:number-table (make-hash-table))
	(set! const:string-table (make-hash-table 'equal))
	(set! const:bytes-table (make-hash-table 'equal))
	(set! const:string-counter 0)
	(set! compiler:static-list null)
	(set! compiler:per-load-static-list null)
	(set! vector-table (make-hash-table))
	(set! new-uninterned-symbols null))

      (define (const:intern-string s)
	(let ([table
	       (if (string? s) 
		   const:string-table 
		   const:bytes-table)])
	  (hash-table-get 
	   table
	   s
	   (lambda ()
	     (begin0
	      const:string-counter
	      (hash-table-put! table s const:string-counter)
	      (set! const:string-counter (add1 const:string-counter)))))))

      (define (compiler:add-per-load-static-list! var)
	(set! compiler:per-load-static-list
	      (cons var compiler:per-load-static-list)))

      (define-values (const:the-per-load-statics-table
		      const:per-load-statics-table?)
	(let-struct const:per-load-statics-table ()
		    (values (make-const:per-load-statics-table)
			    const:per-load-statics-table?)))

      ;; we need to make this in a-normalized, analyzed form from the beginning
      (define compiler:add-const!
	(lambda (code attr)
	  (let* ([var (gensym 'const)]
		 [sv (zodiac:make-top-level-varref 
		      (zodiac:zodiac-stx code)
		      (make-empty-box) 
		      var
		      #f
		      (box '())
		      #f
		      #f
		      #f)]
		 [def (zodiac:make-define-values-form 
		       (zodiac:zodiac-stx code)
		       (make-empty-box) (list sv) code)])
	    
	    (set-annotation! sv (varref:empty-attributes))
	    (varref:add-attribute! sv varref:static)
	    (varref:add-attribute! sv attr)
	    (cond
	     [(eq? attr varref:per-load-static)
	      (set! compiler:per-load-static-list
		    (cons var compiler:per-load-static-list)) 
	      (compiler:add-local-per-load-define-list! def)]
	     [else
	      (set! compiler:static-list (cons var compiler:static-list))
	      (compiler:add-local-define-list! def)])
	    sv)))

      (define compiler:get-special-const!
	(lambda (ast sym attrib table counter)
	  (let ([v (hash-table-get table sym (lambda () #f))])
	    (if v
		(values v counter)
		(let ([sv (zodiac:make-top-level-varref 
			   (and ast (zodiac:zodiac-stx ast))
			   (make-empty-box) 
			   (string->symbol (number->string counter))
			   #f
			   (box '())
			   #f
			   #f
			   #f)])
		  
		  (set-annotation! sv (varref:empty-attributes))
		  (varref:add-attribute! sv attrib)
		  (varref:add-attribute! sv varref:static)

		  (hash-table-put! table sym sv)
		  (values sv (add1 counter)))))))

      (define compiler:get-symbol-const!
	(lambda (ast sym)
	  (let-values ([(sv c) (compiler:get-special-const! ast sym varref:symbol
							    const:symbol-table
							    const:symbol-counter)])
	    (when (c . > . const:symbol-counter)
	      (unless (eq? sym (string->symbol (symbol->string sym)))
		(set! new-uninterned-symbols (cons
					      (cons sym const:symbol-counter)
					      new-uninterned-symbols)))
	      (set! const:symbol-counter c))
	    sv)))

      (define (get-new-uninterned-symbols!)
	(begin0
	 new-uninterned-symbols
	 (set! new-uninterned-symbols null)))

      (define compiler:get-inexact-real-const!
	(lambda (v ast)
	  (let ([sym (string->symbol (number->string v))])
	    (let-values ([(sv c) (compiler:get-special-const! ast sym varref:inexact
							      const:inexact-table
							      const:inexact-counter)])
	      (set! const:inexact-counter c)
	      sv))))

      (define compiler:re-quote 
	(lambda (ast)
	  (zodiac:make-quote-form (zodiac:zodiac-stx ast)
				  (make-empty-box)
				  ast)))

      ;; [make this in analyzed form...]
      (define compiler:make-const-constructor
	(lambda (ast constructor-name args)
	  (let* ([v (zodiac:make-top-level-varref
		     ;; FIXME?: wrong syntax
		     (zodiac:zodiac-stx ast)
		     (make-empty-box) 
		     constructor-name
		     '#%kernel
		     (box '())
		     #f
		     #f
		     #f)]
		 [app  (zodiac:make-app 
			(zodiac:zodiac-stx ast)
			(make-empty-box)
			v
			args)])
	    (set-annotation! v (varref:empty-attributes))
	    (varref:add-attribute! v varref:primitive)
	    (set-annotation! app (make-app #f #t constructor-name))
	    (block:register-max-arity! (get-s:file-block) (length args))
	    (compiler:add-global-varref! v)
	    (compiler:add-primitive-varref! v)
	    app)))

      (define ht-eol (gensym))

      (define (get-hash-id elem)
	(cond
	 [(zodiac:quote-form? elem) (let ([o (zodiac:quote-form-expr elem)])
				      (if (number? (zodiac:zread-object o))
					  (zodiac:zread-object o)
					  o))]
	 [else elem]))

      (define (find-immutable-vector constructor elems)
	(let ([ht (hash-table-get vector-table constructor (lambda () #f))])
	  (and ht
	       (let loop ([ht ht][l elems])
		 (if (null? l)
		     (hash-table-get ht ht-eol (lambda () #f))
		     (let ([ht (hash-table-get ht (get-hash-id (car l)) (lambda () #f))])
		       (and ht (loop ht (cdr l)))))))))
      
      (define (remember-immutable-vector constructor elems const)
	(let ([ht (hash-table-get vector-table constructor make-hash-table)])
	  (hash-table-put! vector-table constructor ht)
	  (let loop ([ht ht][l elems])
	    (if (null? l)
		(hash-table-put! ht ht-eol const)
		(let* ([hash-id (get-hash-id (car l))]
		       [htn (hash-table-get ht hash-id make-hash-table)])
		  (hash-table-put! ht hash-id htn)
		  (loop htn (cdr l)))))))

      (define (construct-vector-constant ast constructor known-immutable?)
	(let* ([elems (map (lambda (x)
			     (compiler:construct-const-code! 
			      (zodiac:make-zread x)
			      known-immutable?))
			   (let ([p (zodiac:zodiac-stx ast)])
			     (or (syntax->list p)
				 (and (vector? (syntax-e p))
				      (vector->list (syntax-e p)))
				 (and (or (regexp? (syntax-e p))
					  (byte-regexp? (syntax-e p)))
				      (list (datum->syntax-object #f (object-name (syntax-e p)))))
				 (let loop ([p p])
				   (cond
				    [(stx-pair? p)
				     (cons (stx-car p)
					   (loop (stx-cdr p)))]
				    [else
				     (list p)])))))]
	       [known-immutable? (or known-immutable? (null? elems))])
	  (or (and known-immutable?
		   (find-immutable-vector constructor elems))
	      (let ([const (compiler:add-const! 
			    (compiler:make-const-constructor 
			     ast
			     constructor
			     elems)
			    (if known-immutable?
				varref:static
				varref:per-load-static))])
		(when known-immutable?
		  (remember-immutable-vector constructor elems const))
		const))))

      (define (big-and-simple/cyclic? datum size ht)
	(cond
	 [(null? datum) (negative? size)]
	 [(hash-table-get ht datum (lambda () #f)) 'cyclic]
	 [(pair? datum)
	  (hash-table-put! ht datum #t)
	  (let ([v (big-and-simple/cyclic? (car datum) 0 ht)])
	    (if (eq? v 'cyclic)
		'cyclic
		(let ([v2 (big-and-simple/cyclic? (cdr datum) (sub1 size) ht)])
		  (if (eq? v2 'cyclic)
		      'cyclic
		      (and v v2)))))]
	 [(vector? datum)
	  (let ([len (vector-length datum)])
	    (and (hash-table-put! ht datum #t)
		 (let loop ([i 0][so-far? #f])
		   (if (= i len)
		       so-far?
		       (let ([v (big-and-simple/cyclic? (vector-ref datum i) (- size i) ht)])
			 (if (eq? v 'cyclic)
			     'cyclic
			     (loop (add1 i) (or so-far? v))))))))]
	 [(hash-table? datum) 'cyclic] ;; assume content is ok and cyclic
	 [(and (negative? size)
	       (or (number? datum)
		   (string? datum)
		   (bytes? datum)
		   (symbol? datum)
		   (boolean? datum)
		   (regexp? datum)
		   (byte-regexp? datum)))
	  #t]
	 [else #f]))
	 
      (define-struct compiled-string (id len))

      (define (construct-big-constant ast stx known-immutable?)
	(let* ([s (let ([p (open-output-bytes)])
		    (write (compile `(quote ,stx)) p)
		    (get-output-bytes p))]
	       [id (const:intern-string s)])
	  (let ([const (compiler:add-const!
			(compiler:re-quote
			 (zodiac:make-zread
			  (datum->syntax-object
			   #f
			   ;; HACK!
			   (make-compiled-string id (bytes-length s)))))
			(if known-immutable?
			    varref:static
			    varref:per-load-static))])
	    const)))

      (define compiler:construct-const-code!
	(lambda (ast known-immutable?)
	  (cond
	   ;; base case - constant does not have to be built
	   [(vm:literal-constant? ast) (compiler:re-quote ast)]

	   ;; c-lambda (kindof a hack)
	   [(c-lambda? ast)
	    (compiler:add-const! (compiler:re-quote 
				  (zodiac:make-zread
				   (datum->syntax-object
				    #f
				    ast ;; See vm2c.ss
				    #f)))
				 varref:static)]
	   
	   ;; a box has a constant inside it to mess with, yet it's
	   ;; still a scalar
	   [(box? (zodiac:zread-object ast))
	    (compiler:add-const! (compiler:make-const-constructor
				  ast
				  'box
				  (list (compiler:construct-const-code!
					 (zodiac:make-zread (unbox (zodiac:zread-object ast)))
					 known-immutable?)))
				 (if known-immutable?
				     varref:static
				     varref:per-load-static))]

	   ;; Do symbols at most once:
	   [(symbol? (zodiac:zread-object ast))
	    (let ([sym (zodiac:zread-object ast)])
	      (compiler:get-symbol-const! ast sym))]
	   
	   ;; Numbers that must be built
	   [(number? (zodiac:zread-object ast))
	    (let ([n (zodiac:zread-object ast)])
	      (if (and (inexact? n) (eqv? 0 (imag-part n))
		       (not (member n '(+inf.0 -inf.0 +nan.0 -0.0))))
		  (compiler:get-inexact-real-const! n ast)
		  (let ([sym (string->symbol (number->string n))])
		    (hash-table-get const:number-table
				    sym
				    (lambda ()
				      (let ([num (compiler:add-const! 
						  (compiler:re-quote ast) 
						  varref:static)])
					(hash-table-put! const:number-table sym num)
					num))))))]

	   ;; big/cyclic constants
	   [(big-and-simple/cyclic? (syntax-object->datum (zodiac:zodiac-stx ast)) 20 (make-hash-table))
	    (construct-big-constant ast (zodiac:zodiac-stx ast) known-immutable?)]
	   
	   ;; lists
	   [(stx-list? (zodiac:zodiac-stx ast))
	    (construct-vector-constant ast 'list known-immutable?)]
	   
	   ;; improper lists
	   [(pair? (zodiac:zread-object ast))
	    (construct-vector-constant ast 'list* known-immutable?)]

	   [(void? (zodiac:zread-object ast))
	    (zodiac:make-special-constant 'void)]

	   ;; vectors
	   [(vector? (zodiac:zread-object ast))
	    (construct-vector-constant ast 'vector known-immutable?)]
	   
	   ;; regexp
	   [(regexp? (zodiac:zread-object ast))
	    (construct-vector-constant ast 'regexp #t)]
	   [(byte-regexp? (zodiac:zread-object ast))
	    (construct-vector-constant ast 'byte-regexp #t)]
	   
	   ;; comes from module paths in analyze:
	   [(module-path-index? (zodiac:zread-object ast))
	    (let-values ([(path base) (module-path-index-split (zodiac:zread-object ast))])
	      (if (or path base)
		  (let ([wrap (lambda (v)
				(zodiac:make-zread 
				 (datum->syntax-object
				  #f
				  v
				  (zodiac:zodiac-stx ast))))])
		    (compiler:add-const! (compiler:make-const-constructor
					  ast
					  'module-path-index-join
					  (list (compiler:construct-const-code!
						 (wrap path)
						 known-immutable?)
						(compiler:construct-const-code!
						 (wrap base)
						 known-immutable?)))
					 (if known-immutable?
					     varref:static
					     varref:per-load-static)))
		  (zodiac:make-special-constant 'self_modidx)))]

	   ;; other atomic constants that must be built
	   [else
	    (when (or (string? (zodiac:zread-object ast))
		      (bytes? (zodiac:zread-object ast)))
	      (const:intern-string (zodiac:zread-object ast)))
	    (compiler:add-const! (compiler:re-quote ast) 
				 varref:static)])))))

