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
  (require (lib "unitsig.ss")
	   (lib "list.ss")
	   (lib "etc.ss"))

  (require (lib "zodiac-sig.ss" "syntax")
	   (lib "stx.ss" "syntax"))

  (require "sig.ss")
  (require "../sig.ss")

  (provide const@)
  (define const@
    (unit/sig compiler:const^
      (import (compiler:option : compiler:option^)
	      compiler:library^
	      compiler:cstructs^
	      (zodiac : zodiac^)
	      compiler:analyze^
	      compiler:zlayer^
	      compiler:vmstructs^
	      compiler:top-level^
	      compiler:driver^)
      
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
      (define compiler:per-invoke-static-list null)

      (define (compiler:get-static-list) compiler:static-list)
      (define (compiler:get-per-load-static-list) compiler:per-load-static-list)
      (define (compiler:get-per-invoke-static-list) compiler:per-invoke-static-list)

      (define new-uninterned-symbols null) ; list of (cons sym pos)

      (define syntax-strings null) ; list of syntax-string structs

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
	(set! compiler:per-invoke-static-list null)
	(set! vector-table (make-hash-table))
	(set! new-uninterned-symbols null)
	(set! syntax-strings null))

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

      (define (compiler:add-per-invoke-static-list! var mi)
	(set! compiler:per-invoke-static-list
	      (cons (cons var mi) compiler:per-invoke-static-list)))

      (define-values (const:the-per-load-statics-table
		      const:per-load-statics-table?)
	(let-struct const:per-load-statics-table ()
		    (values (make-const:per-load-statics-table)
			    const:per-load-statics-table?)))

      (define (wrap-module-definition def mi)
	(let ([def (zodiac:make-module-form
		    (zodiac:zodiac-stx def)
		    (make-empty-box)
		    #f #f #f #f
		    def #f
		    #f #f #f #f #f)])
	  (set-annotation! 
	   def 
	   (make-module-info mi 
			     #f
			     (if (varref:module-invoke-syntax? mi)
				 'syntax-body
				 'body)))
	  def))

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
	     [(varref:module-invoke? attr)
	      (set! compiler:per-invoke-static-list
		    (cons (cons var attr) compiler:per-invoke-static-list)) 
	      (let ([def (wrap-module-definition def attr)])
		(compiler:add-local-per-invoke-define-list! def))]
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

      (define-struct syntax-string (str mi uposes ustart id))

      (define (compiler:add-syntax-string! str mi uninterned-positions uninterned-start)
	(let ([naya (make-syntax-string str mi uninterned-positions uninterned-start
					(length syntax-strings))])
	  (set! syntax-strings (cons naya syntax-strings))
	  naya))

      (define (const:get-syntax-strings)
	syntax-strings)

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
					 (or (varref:current-invoke-module)
					     (if known-immutable?
						 varref:static
						 varref:per-load-static))))
		  (zodiac:make-special-constant 'self_modidx)))]

	   ;; other atomic constants that must be built
	   [else
	    (when (or (string? (zodiac:zread-object ast))
		      (bytes? (zodiac:zread-object ast)))
	      (const:intern-string (zodiac:zread-object ast)))
	    (compiler:add-const! (compiler:re-quote ast) 
				 varref:static)])))

      (define syntax-constants null)

      (define (const:reset-syntax-constants!)
	(set! syntax-constants null))

      (define (const:make-syntax-constant stx)
	;; Marhsall to a string constant, and read back out at run-time.
	;;  For sharing of syntax info, put all syntax objects for a given
	;;  top-level expression into one marshal step.
	(let* ([var (gensym 'conststx)]
	       [sv (zodiac:make-top-level-varref 
		    stx
		    (make-empty-box) 
		    var
		    #f
		    (box '())
		    #f
		    #f
		    #f)])
	  (set! syntax-constants (cons (cons sv stx)
				       syntax-constants))
	  (set-annotation! sv (varref:empty-attributes))
	  (varref:add-attribute! sv varref:static)
	  (varref:add-attribute! sv (or (varref:current-invoke-module)
					varref:per-load-static))
	  (if (varref:current-invoke-module)
	      (set! compiler:per-invoke-static-list
		    (cons (cons var (varref:current-invoke-module))
			  compiler:per-invoke-static-list))
	      (set! compiler:per-load-static-list
		    (cons var compiler:per-load-static-list)))
	  sv))

      ;; We collect syntax objects together to share the cost of of
      ;; the rename tables. More gnerally, to get the expansion-time
      ;; info to use-time, we use the bytecode writer built into
      ;; MzScheme, putting multiple syntax objects together into a
      ;; syntax vector. The scheme_eval_compiled_stx_string() will
      ;; unpack it, and perform any necessary phase shifts. To perform
      ;; the module mapping associated with the phase shift,
      ;; scheme_eval_compiled_stx_string() expects the "syntax" vector
      ;; to have a module index path (the "self" path) as its last
      ;; element.
      ;; Returns a max-arity.
      (define (const:finish-syntax-constants!)
	(if (null? syntax-constants)
	    0
	    (let* ([s (open-output-bytes)]
		   [uninterned-symbol-info (get-new-uninterned-symbols!)]
		   [c (compile `(quote-syntax ,(list->vector 
						(let ([l (map cdr syntax-constants)]
						      [mi (varref:current-invoke-module)])
						  (append 
						   l 
						   (map car uninterned-symbol-info) ; car gets the syms
						   (if mi
						       (list (varref:module-invoke-context-path-index mi))
						       null))))))])
	      (display c s)
	      (let ([syntax-string (get-output-bytes s)])
		(let* ([strvar (compiler:add-syntax-string! 
				syntax-string
				(varref:current-invoke-module)
				(map cdr uninterned-symbol-info) ; cdr gets positions
				(length syntax-constants))] ; starting place for symbols
		       [vecvar (gensym 'conststxvec)]
		       [sv (zodiac:make-top-level-varref
			    #f
			    (make-empty-box) 
			    vecvar
			    #f
			    (box '())
			    #f
			    #f
			    #f)])

		  (set-annotation! sv (varref:empty-attributes))
		  (varref:add-attribute! sv varref:static)
		  (varref:add-attribute! sv (or (varref:current-invoke-module)
						varref:per-load-static))
		  (if (varref:current-invoke-module)
		      (set! compiler:per-invoke-static-list
			    (cons (cons vecvar (varref:current-invoke-module))
				  compiler:per-invoke-static-list))
		      (set! compiler:per-load-static-list
			    (cons vecvar compiler:per-load-static-list)))

		  ((if (varref:current-invoke-module)
		       compiler:add-local-per-invoke-define-list!
		       compiler:add-local-per-load-define-list!)
		   (let ([def
			  (zodiac:make-define-values-form 
			   #f
			   (make-empty-box) (list sv)
			   (compiler:re-quote 
			    (zodiac:make-zread
			     (datum->syntax-object
			      #f
			      strvar ;; <------ HACK! See "HACK!" in vm2c.ss
			      #f))))])
		     (if (varref:current-invoke-module)
			 (wrap-module-definition def (varref:current-invoke-module))
			 def)))

		  ;; Create construction code for each
		  ;;  syntax variable:
		  (let loop ([l syntax-constants]
			     [pos 0])
		    (unless (null? l)
		      (let ([app (zodiac:make-app
				  (cdar l)
				  (make-empty-box) 
				  (zodiac:make-top-level-varref
				   (cdar l)
				   (make-empty-box) 
				   'vector-ref
				   '#%kernel
				   (box '())
				   #f
				   #f
				   #f)
				  (list
				   sv
				   (compiler:re-quote
				    (zodiac:make-zread
				     (datum->syntax-object
				      #f
				      pos
				      (cdar l))))))])
			(set-annotation! app (make-app #f #t 'vector-ref))
			((if (varref:current-invoke-module)
			     compiler:add-local-per-invoke-define-list!
			     compiler:add-local-per-load-define-list!)
			 (let ([def
				(zodiac:make-define-values-form 
				 (cdar l)
				 (make-empty-box) (list (caar l))
				 app)])
			   (if (varref:current-invoke-module)
			       (wrap-module-definition def (varref:current-invoke-module))
			       def)))
			(loop (cdr l) (add1 pos)))))))
	      (set! syntax-constants null)
	      ;; We make an application with 2 arguments
	      2))))))

