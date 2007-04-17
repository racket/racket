(module serialize mzscheme
  (require-for-syntax (lib "struct.ss" "syntax"))
  (require (lib "modcollapse.ss" "syntax")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "private/serialize-structs.ss")

  (provide define-serializable-struct
	   define-serializable-struct/versions

	   ;; For implementors of other `define-struct'-like forms:
	   prop:serializable
	   make-serialize-info
	   make-deserialize-info

	   ;; Checks whether a value is seriliazable:
	   serializable?

	   ;; The two main routines:
	   serialize
	   deserialize)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; define-serializable-struct
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; generate-struct-declaration wants a function to generate the actual
  ;;  call to `make-struct-type'. This is where we insert the serializable property.
  (define-for-syntax (make-make-make-struct-type inspector+deserializer-stx)
    (let-values ([(inspector-stx deserialize-id)
		  (apply values (syntax->list inspector+deserializer-stx))])
      (lambda (orig-stx name-stx defined-name-stxes super-info)
	(when super-info
	  (unless (andmap values (list-ref super-info 3))
	    (raise-syntax-error
	     #f
	     "not all fields are known for parent struct type"
	     orig-stx
	     (syntax-case orig-stx ()
	       [(_ (__ super-id) . rest) #'super-id]))))
	(let ([num-fields (/ (- (length defined-name-stxes) 3) 2)])
	  #`(letrec-values ([(type maker pred access mutate)
			     (make-struct-type '#,name-stx 
					       #,(and super-info (list-ref super-info 0))
					       #,num-fields
					       0 #f
					       (list
						;; --- The prop:serializable property means this is serializable
						(cons
						 prop:serializable
						 (make-serialize-info
						  ;; The struct-to-vector function: --------------------
						  (lambda (v)
						    (vector
						     #,@(if super-info
							    (reverse
							     (map (lambda (sel)
								    #`(#,sel v))
								  (list-ref super-info 3)))
							    null)
						     #,@(let loop ([n num-fields][r null])
							  (if (zero? n)
							      r
							      (loop (sub1 n)
								    (cons
								     #`(access v #,(sub1 n))
								     r))))))
						  ;; The serializer id: --------------------
						  (quote-syntax #,deserialize-id)
						  ;; Alternate --- does the same thing, due to the way
						  ;;  this macro is set up:
						  #;
						  (let ([b (identifier-binding (quote-syntax #,deserialize-id))])
						    (if (list? b)
							(cons '#,deserialize-id (caddr b))
							'#,deserialize-id))
						  ;; Can handle cycles? --------------------
						  ;;  Yes, as long as we have mutators for the
						  ;;  superclass.
						  #,(or (not super-info)
							(andmap values (list-ref super-info 4)))
						  ;; Directory for last-ditch resolution --------------------
						  (or (current-load-relative-directory) 
						      (current-directory))))
						;; --- The prop:internal-deserialize property just communicates
						;;     information to the deserialize binding (because it's more
						;;     convenient to generate the deserialize info here)
						(cons
						 prop:internal-deserialize
						 (list
						  ;; The maker-getter: --------------------
						  (lambda () maker)
						  ;; The shell function: --------------------
						  ;;  Returns an shell object plus
						  ;;  a function to update the shell (used for
						  ;;  building cycles): 
						  (let ([super-sets
							 (list #,@(if super-info
								      (list-ref super-info 4)
								      null))])
						    (lambda ()
						      (let ([s0
							     (#,(list-ref defined-name-stxes 1)
							      #,@(append
								  (if super-info
								      (map (lambda (x) #f)
									   (list-ref super-info 3))
								      null)
								  (vector->list
								   (make-vector num-fields #f))))])
							(values
							 s0
							 (lambda (s)
							   #,@(if super-info
								  (map (lambda (set get)
									 #`(#,set s0 (#,get s)))
								       (list-ref super-info 4)
								       (list-ref super-info 3))
								  null)
							   #,@(let loop ([n num-fields])
								(if (zero? n)
								    null
								    (let ([n (sub1 n)])
								      (cons #`(mutate s0 #,n (access s #,n))
									    (loop n)))))
							   (void)))))))))
					       #,inspector-stx)])
	      (values type maker pred access mutate))))))
    
  (define-syntaxes (define-serializable-struct define-serializable-struct/versions)
    (let ()
      (define expected-ids
	"expected an identifier or parenthesized sequence of struct identifier and parent identifier")
      (define expected-fields
	"expected parenthesized sequence of field identifiers")
      (define expected-version
	"expected a version (literal, exact, non-negative integer)")
      (define (after-name id/sup-stx)
	(if (identifier? id/sup-stx)
	    " after struct identifier"
	    " after sequence of struct and parent identifiers"))
      (define version-conflicts
	"version number for other-version deserializer conflicts with")
      
      (define (context-check stx)
	(unless (memq (syntax-local-context) '(top-level module))
	  (raise-syntax-error
	   #f
	   "allowed only at the top level or within a module top level"
	   stx)))

      (define (ok-version? v)
	(and (number? v)
	     (integer? v)
	     (v . >= . 0)
	     (exact? v)))

      ;; ------------------------------
      ;;  Main parsing
      
      ;; Check high-level syntax, then dispatch to parsing parts
      (define main
	(lambda (stx)
	  (syntax-case stx ()
	    [(_ id/sup fields)
	     (parse stx #'id/sup #'0 #'fields #'() #'(current-inspector))]
	    [(_ id/sup fields inspector-expr)
	     (parse stx #'id/sup #'0 #'fields #'() #'inspector-expr)]
	    [(_ id/sup)
	     (parse stx #'id/sup #'0 #f #f #f)]
	    [(_)
	     (raise-syntax-error
	      #f
	      expected-ids
	      stx)])))

      ;; Check high-level syntax with versions, then dispatch to parsing parts
      (define main/versions
	(lambda (stx)
	  (syntax-case stx ()
	    [(_ id/sup version-num fields other-versions)
	     (parse stx #'id/sup #'version-num #'fields #'other-versions #'(current-inspector))]
	    [(_ id/sup version-num fields other-versions inspector-expr)
	     (parse stx #'id/sup #'version-num #'fields #'other-versions #'inspector-expr)]
	    [(_ id/sup)
	     (parse stx #'id/sup #f #f #f #f)]
	    [(_ id/sup version-num)
	     (parse stx #'id/sup #'version-num #f #f #f)]
	    [(_ id/sup version-num fields)
	     (parse stx #'id/sup #'version-num #'fields #f #f)]
	    [(_)
	     (raise-syntax-error
	      #f
	      expected-ids
	      stx)])))
      
      ;; ------------------------------
      ;;  Part parsing

      ;; Parse parts, then dispatch to result generation
      (define (parse stx id/sup-stx version-num-stx fields-stx other-versions-stx inspector-stx)
	;; First, check id or id+super:
	(let-values ([(id super-id) (parse-id+super stx id/sup-stx)])
	  (let* ([version-num (parse-version stx id/sup-stx version-num-stx)]
		 [field-ids (parse-fields stx id/sup-stx version-num-stx fields-stx)]
		 [other-versions (parse-other-versions stx version-num other-versions-stx)])
	    ;; Input syntax is ok! Generate the results
	    #`(begin
		#,(generate-main-result stx id super-id field-ids inspector-stx version-num)
		#,@(map (lambda (other-version)
			  (generate-other-result stx id other-version))
			other-versions)))))

      ;; id+super
      (define (parse-id+super stx id/sup-stx)
	(syntax-case id/sup-stx ()
	  [id
	   (identifier? #'id)
	   (values #'id #f)]
	  [(id sup-id)
	   (and (identifier? #'id)
		(identifier? #'sup-id))
	   (values #'id #'sup-id)]
	  [(id other)
	   (identifier? #'id)
	   (raise-syntax-error
	    #f
	    "expected identifier for parent struct type"
	    stx
	    #'other)]
	  [else
	   (raise-syntax-error
	    #f
	    expected-ids
	    stx
	    id/sup-stx)]))

      ;; version
      (define (parse-version stx id/sup-stx version-num-stx)
	;; Check version; #f means no version in original expression
	(unless version-num-stx
	  (raise-syntax-error
	   #f
	   (string-append expected-version
			  (after-name id/sup-stx))
	   stx))
	(let ([v (syntax-e version-num-stx)])
	  (unless (ok-version? v)
	    (raise-syntax-error
	     #f
	     (string-append expected-version
			    (after-name id/sup-stx))
	     stx
	     version-num-stx))
	  v))

      ;; fields
      (define (parse-fields stx id/sup-stx version-num-stx fields-stx)
	;; Now check fields; #f means no fields in oirignal expression
	(unless fields-stx
	  (raise-syntax-error
	   #f
	   (string-append expected-fields
			  (cond
			   [version-num-stx
			    " after version number"]
			   [else (after-name id/sup-stx)]))
	   stx))
	(let ([field-ids (syntax-case fields-stx ()
			   [(field ...)
			    (let ([field-ids (syntax->list #'(field ...))])
			      (for-each (lambda (id)
					  (unless (identifier? id)
					      (raise-syntax-error
					       #f
					       "expected a field identifier"
					       stx
					       id)))
					field-ids)
			      field-ids)]
			   [else
			    (raise-syntax-error
			     #f
			     expected-fields
			     stx
			     fields-stx)])])
	  ;; Fields are all identifiers, so check for distinct fields
	  (let ([dup (check-duplicate-identifier field-ids)])
	    (when dup
	      (raise-syntax-error
	       #f
	       "duplicate field identifier"
	       stx
	       dup)))
	  field-ids))

      ;; Other-version deserializers
      (define (parse-other-versions stx main-version-num other-versions-stx)
	(when (or (not other-versions-stx)
		  (not (syntax->list other-versions-stx)))
	  (raise-syntax-error
	   #f
	   "expected a parenthesized sequence of other-version deserializers after field sequence"
	   stx
	   other-versions-stx))
	(let* ([ht (make-hash-table 'equal)]
	       [other-versions
		(map (lambda (other-stx)
		       (syntax-case other-stx ()
			 [(version-num maker-expr cycle-maker-expr)
			  (let ([v (syntax-e #'version-num)])
			    (unless (ok-version? v)
			      (raise-syntax-error
			       #f
			       (string-append expected-version
					      " for other-version deserializer")
			       stx
			       #'version-num))
			    (when (= v main-version-num)
			      (raise-syntax-error
			       #f
			       (string-append version-conflicts " the main version")
			       stx
			       other-stx))
			    (when (hash-table-get ht v (lambda () #f))
			      (raise-syntax-error
			       #f
			       (string-append version-conflicts " another deserializer")
			       stx
			       other-stx))
			    (hash-table-put! ht v #t)
			    (list v #'maker-expr #'cycle-maker-expr))]
			 [else
			  (raise-syntax-error
			   #f
			   "expected a deserializer for another version: version number, constructor, and cycle constructor"
			   stx
			   other-stx)]))
		     (syntax->list other-versions-stx))])
	  other-versions))

      ;; ------------------------------
      ;;  Generate result

      ;; Generate the result expression. This is complicated
      ;; by the fact that super-id information may or may not be
      ;; immediately available, so we also need continue-define-...
      (define (generate-main-result stx id super-id field-ids inspector-stx version-num)
	(with-syntax ([deserializer-id (make-deserialize-name id version-num)]
		      [struct-type-id (datum->syntax-object
				       id
				       (string->symbol
					(format "struct:~a" (syntax-e id)))
				       id)]
		      [inspector-expr inspector-stx])
	  #`(begin
	      #,(generate-struct-declaration stx
					     id super-id field-ids 
					     (syntax-local-context)
					     (make-make-make-struct-type #'(inspector-expr deserializer-id)))
	      (define deserializer-id (let ([l (internal-deserialize-info struct-type-id)])
					(make-deserialize-info
					 ((car l))
					 (cadr l))))
	      #,@(make-deserialize-provide stx #'deserializer-id))))

      (define (generate-other-result stx id other-version)
	(with-syntax ([deserializer-id (make-deserialize-name id (car other-version))])
	  #`(begin
	      (define deserializer-id (make-deserialize-info #,(cadr other-version)
							     #,(caddr other-version)))
	      #,@(make-deserialize-provide stx #'deserializer-id))))

      (define (make-deserialize-name id version-num)
	(datum->syntax-object
	 id
	 (string->symbol
	  (format "deserialize-info:~a-v~a" 
		  (syntax-e id) 
		  version-num))
	 id))

      (define (make-deserialize-provide stx deserializer-id-stx)
	(if (eq? 'top-level (syntax-local-context))
	    ;; Top level; in case deserializer-id-stx is macro-introduced,
	    ;;  explicitly use namespace-set-variable-value!
	    (list (quasisyntax/loc stx
		    (namespace-set-variable-value! '#,deserializer-id-stx 
						   #,deserializer-id-stx )))
	    ;; In a module; provide:
	    (list (quasisyntax/loc stx
		    (provide #,deserializer-id-stx)))))

      ;; ------------------------------
      ;;  The transformers

      (values
       (lambda (stx)
	 (context-check stx)
	 (main stx))
       (lambda (stx)
	 (context-check stx)
	 (main/versions stx)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; serialize
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (serializable? v)
    (or (serializable-struct? v)
	(boolean? v)
	(null? v)
	(number? v)
	(char? v)
	(symbol? v)
	(string? v)
	(path-for-some-system? v)
	(bytes? v)
	(vector? v)
	(pair? v)
	(hash-table? v)
	(box? v)
	(void? v)
	(date? v)
	(arity-at-least? v)))

  ;; If a module is dynamic-required through a path,
  ;;  then it can cause simplified module paths to be paths;
  ;;  keep the literal path, but marshal it to bytes.
  (define (protect-path p)
    (if (path? p)
	(path->bytes p)
	p))
  (define (unprotect-path p)
    (if (bytes? p)
	(bytes->path p)
	p))
  
  (define (mod-to-id info mod-map cache)
    (let ([deserialize-id (serialize-info-deserialize-id info)])
      (hash-table-get 
       cache deserialize-id
       (lambda ()
	 (let ([id
		(let ([path+name
		       (cond
			[(identifier? deserialize-id)
			 (let ([b (identifier-binding deserialize-id)])
			   (cons
			    (and (list? b)
				 (if (symbol? (caddr b))
				     (caddr b)
				     (protect-path
				      (collapse-module-path-index 
				       (caddr b)
				       (build-path (serialize-info-dir info)
						   "here.ss")))))
			    (syntax-e deserialize-id)))]
			[(symbol? deserialize-id)
			 (cons #f deserialize-id)]
			[else
			 (cons
			  (if (symbol? (cdr deserialize-id))
			      (cdr deserialize-id)
			      (protect-path
			       (collapse-module-path-index 
				(cdr deserialize-id)
				(build-path (serialize-info-dir info)
					    "here.ss"))))
			  (car deserialize-id))])])
		  (hash-table-get 
		   mod-map path+name
		   (lambda ()
		     (let ([id (hash-table-count mod-map)])
		       (hash-table-put! mod-map path+name id)
		       id))))])
	   (hash-table-put! cache deserialize-id id)
	   id)))))

  (define (is-mutable? o)
    (or (and (or (pair? o)
		 (box? o)
		 (vector? o)
		 (date? o)
		 (hash-table? o)
		 (arity-at-least? o))
	     (not (immutable? o)))
	(serializable-struct? o)))

  ;; Finds a mutable object among those that make the
  ;;  current cycle.
  (define (find-mutable v cycle-stack) 
    ;; Walk back through cycle-stack to find something
    ;;  mutable. If we get to v without anything being
    ;;  mutable, then we're stuck.
    (let ([o (car cycle-stack)])
      (cond
       [(eq? o v)
	(error 'serialize "cannot serialize cycle of immutable values: ~e" v)]
       [(is-mutable? o)
	o]
       [else
	(find-mutable v (cdr cycle-stack))])))


  (define (share-id share cycle)
    (+ (hash-table-count share)
       (hash-table-count cycle)))

  ;; Traverses v to find cycles and charing. Shared
  ;;  object go in the `shared' table, and cycle-breakers go in
  ;;  `cycle'. In each case, the object is mapped to a number that is
  ;;  incremented as shared/cycle objects are discovered, so
  ;;  when the objects are deserialized, build them in reverse
  ;;  order.
  (define (find-cycles-and-sharing v cycle share)
    (let ([tmp-cycle (make-hash-table)]  ;; candidates for sharing
	  [tmp-share (make-hash-table)]  ;; candidates for cycles
	  [cycle-stack null])            ;; same as in tmpcycle, but for finding mutable
      (let loop ([v v])
	(cond
	 [(or (boolean? v)
	      (number? v)
	      (char? v)
	      (symbol? v)
	      (null? v)
	      (void? v))
	  (void)]
	 [(hash-table-get cycle v (lambda () #f))
	  ;; We already know that this value is
	  ;;  part of a cycle
	  (void)]
	 [(hash-table-get tmp-cycle v (lambda () #f))
	  ;; We've just learned that this value is
	  ;;  part of a cycle.
	  (let ([mut-v (if (is-mutable? v)
			   v
			   (find-mutable v cycle-stack))])
	    (hash-table-put! cycle mut-v (share-id share cycle))
	    (unless (eq? mut-v v)
	      ;; This value is potentially shared
	      (hash-table-put! share v (share-id share cycle))))]
	 [(hash-table-get share v (lambda () #f))
	  ;; We already know that this value is shared
	  (void)]
	 [(hash-table-get tmp-share v (lambda () #f))
	  ;; We've just learned that this value is
	  ;;  shared
	  (hash-table-put! share v (share-id share cycle))]
	 [else
	  (hash-table-put! tmp-share v #t)
	  (hash-table-put! tmp-cycle v #t)
	  (set! cycle-stack (cons v cycle-stack))
	  (cond
	   [(serializable-struct? v)
	    (let ([info (serializable-info v)])
	      (for-each loop (vector->list ((serialize-info-vectorizer info) v))))]
	   [(or (string? v)
		(bytes? v)
		(path-for-some-system? v))
	    ;; No sub-structure
	    (void)]
	   [(vector? v)
	    (for-each loop (vector->list v))]
	   [(pair? v)
	    (loop (car v)) 
	    (loop (cdr v))]
	   [(box? v)
	    (loop (unbox v))]
	   [(date? v)
	    (for-each loop (cdr (vector->list (struct->vector v))))]
	   [(hash-table? v)
	    (hash-table-for-each v (lambda (k v)
				     (loop k)
				     (loop v)))]
	   [(arity-at-least? v)
	    (loop (arity-at-least-value v))]
	   [else (raise-type-error
		  'serialize
		  "serializable object"
		  v)])
	  ;; No more possibility for this object in
	  ;;  a cycle:
	  (hash-table-remove! tmp-cycle v)
	  (set! cycle-stack (cdr cycle-stack))]))))

  (define (serialize-one v share check-share? mod-map mod-map-cache)
    (define ((serial check-share?) v)
      (cond
       [(or (boolean? v)
	    (number? v)
	    (char? v)
	    (symbol? v)
	    (null? v))
	v]
       [(void? v)
	'(void)]
       [(and check-share?
	     (hash-table-get share v (lambda () #f)))
	=> (lambda (v) (cons '? v))]
       [(and (or (string? v)
		 (bytes? v))
	     (immutable? v))
	v]
       [(serializable-struct? v)
	(let ([info (serializable-info v)])
	  (cons (mod-to-id info mod-map mod-map-cache) 
		(map (serial #t)
		     (vector->list
		      ((serialize-info-vectorizer info) v)))))]
       [(or (string? v)
	    (bytes? v))
	(cons 'u v)]
       [(path-for-some-system? v)
	(list* 'p+ (path->bytes v) (path-convention-type v))]
       [(vector? v)
	(cons (if (immutable? v) 'v 'v!)
	      (map (serial #t) (vector->list v)))]
       [(pair? v)
	(let ([loop (serial #t)])
	  (cons (if (immutable? v) 'c 'c!)
		(cons (loop (car v)) 
		      (loop (cdr v)))))]
       [(box? v)
	(cons (if (immutable? v) 'b 'b!)
	      ((serial #t) (unbox v)))]
       [(hash-table? v)
	(list* 'h
	       (if (immutable? v) '- '!)
	       (append
		(if (hash-table? v 'equal) '(equal) null)
		(if (hash-table? v 'weak) '(weak) null))
	       (let ([loop (serial #t)])
		 (hash-table-map v (lambda (k v)
				     (cons (loop k)
					   (loop v))))))]
       [(date? v)
	(cons 'date
	      (map (serial #t) (cdr (vector->list (struct->vector v)))))]
       [(arity-at-least? v)
	(cons 'arity-at-least
	      ((serial #t) (arity-at-least-value v)))]
       [else (error 'serialize "shouldn't get here")]))
    ((serial check-share?) v))
  
  (define (serial-shell v mod-map mod-map-cache)
    (cond
     [(serializable-struct? v)
      (let ([info (serializable-info v)])
	(mod-to-id info mod-map mod-map-cache))]
     [(vector? v)
      (cons 'v (vector-length v))]
     [(pair? v)
      'c]
     [(box? v)
      'b]
     [(hash-table? v)
      (cons 'h (append
		(if (hash-table? v 'equal) '(equal) null)
		(if (hash-table? v 'weak) '(weak) null)))]
     [(date? v)
      'date]
     [(arity-at-least? v)
      'arity-at-least]))

  (define (serialize v)
    (let ([mod-map (make-hash-table)]
	  [mod-map-cache (make-hash-table 'equal)]
	  [share (make-hash-table)]
	  [cycle (make-hash-table)])
      ;; First, traverse V to find cycles and sharing
      (find-cycles-and-sharing v cycle share)
      ;; To simplify, all add the cycle records to shared.
      ;;  (but keep cycle info, too).
      (hash-table-for-each cycle
			   (lambda (k v)
			     (hash-table-put! share k v)))
      (let ([ordered (map car (sort (hash-table-map share cons)
                                    (lambda (a b) (< (cdr a) (cdr b)))))])
	(let ([serializeds (map (lambda (v)
				  (if (hash-table-get cycle v (lambda () #f))
				      ;; Box indicates cycle record allocation
				      ;;  followed by normal serialization
				      (box (serial-shell v mod-map mod-map-cache))
				      ;; Otherwise, normal serialization
				      (serialize-one v share #f mod-map mod-map-cache)))
				ordered)]
	      [fixups (hash-table-map 
		       cycle
		       (lambda (v n)
			 (cons n
			       (serialize-one v share #f mod-map mod-map-cache))))]
	      [main-serialized (serialize-one v share #t mod-map mod-map-cache)]
	      [mod-map-l (map car (sort (hash-table-map mod-map cons)
                                        (lambda (a b) (< (cdr a) (cdr b)))))])
	  (list (hash-table-count mod-map)
		mod-map-l
		(length serializeds)
		serializeds
		fixups
		main-serialized)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; deserialize
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-values (prop:internal-deserialize internal-deserialize? internal-deserialize-info)
    (make-struct-type-property 'internal-deserialize #f))

  (define (deserialize-one v share mod-map)
    (let loop ([v v])
      (cond
       [(or (boolean? v)
	    (number? v)
	    (char? v)
	    (symbol? v)
	    (null? v))
	v]
       [(string? v)
	(string->immutable-string v)]
       [(bytes? v)
	(bytes->immutable-bytes v)]
       [(number? (car v))
	;; Struct instance:
	(let ([info (vector-ref mod-map (car v))])
	  (apply (deserialize-info-maker info) (map loop (cdr v))))]
       [else
	(case (car v)
	  [(?) (vector-ref share (cdr v))]
	  [(void) (void)]
	  [(u) (let ([x (cdr v)])
		 (cond
		  [(string? x) (string-copy x)]
		  [(bytes? x) (bytes-copy x)]))]
	  [(p) (bytes->path (cdr v))]
	  [(p+) (bytes->path (cadr v) (cddr v))]
	  [(c) (cons-immutable (loop (cadr v)) (loop (cddr v)))]
	  [(c!) (cons (loop (cadr v)) (loop (cddr v)))]
	  [(v) (apply vector-immutable (map loop (cdr v)))]
	  [(v!) (list->vector (map loop (cdr v)))]
	  [(b) (box-immutable (loop (cdr v)))]
	  [(b!) (box (loop (cdr v)))]
	  [(h) (let ([al (map (lambda (p)
				(cons (loop (car p))
				      (loop (cdr p))))
			      (cdddr v))])
		 (if (eq? '! (cadr v))
		     (let ([ht (apply make-hash-table (caddr v))])
		       (for-each (lambda (p)
				   (hash-table-put! ht (car p) (cdr p)))
				 al)
		       ht)
		     (apply make-immutable-hash-table al (caddr v))))]
	  [(date) (apply make-date (map loop (cdr v)))]
	  [(arity-at-least) (make-arity-at-least (loop (cdr v)))]
	  [else (error 'serialize "ill-formed serialization")])])))

  (define (deserial-shell v mod-map fixup n)
    (cond
     [(number? v)
      ;; Struct instance
      (let* ([info (vector-ref mod-map v)])
	(let-values ([(obj fix) ((deserialize-info-cycle-maker info))])
	  (vector-set! fixup n fix)
	  obj))]
     [(pair? v)
      (case (car v)
	[(v)
	 ;; Vector 
	 (let* ([m (cdr v)]
		[v0 (make-vector m #f)])
	   (vector-set! fixup n (lambda (v)
				  (let loop ([i m])
				    (unless (zero? i)
				      (let ([i (sub1 i)])
					(vector-set! v0 i (vector-ref v i))
					(loop i))))))
	   v0)]
	[(h)
	 ;; Hash table
	 (let ([ht0 (make-hash-table)])
	   (vector-set! fixup n (lambda (ht)
				  (hash-table-for-each 
				   ht
				   (lambda (k v)
				     (hash-table-put! ht0 k v)))))
	   ht0)])]
     [else
      (case v
	[(c) 
	 (let ([p0 (cons #f #f)])
	   (vector-set! fixup n (lambda (p)
				  (set-car! p0 (car p))
				  (set-cdr! p0 (cdr p))))
	   p0)]
	[(b)
	 (let ([b0 (box #f)])
	   (vector-set! fixup n (lambda (b)
				  (set-box! b0 (unbox b))))
	   b0)]
	[(date)
	 (let ([d0 (make-date #f #f #f #f #f #f #f #f #f #f)])
	   (vector-set! fixup n (lambda (d)
				  (set-date-second! d0 (date-second d))
				  (set-date-minute! d0 (date-minute d))
				  (set-date-hour! d0 (date-hour d))
				  (set-date-day! d0 (date-day d))
				  (set-date-month! d0 (date-month d))
				  (set-date-year! d0 (date-year d))
				  (set-date-week-day! d0 (date-week-day d))
				  (set-date-year-day! d0 (date-year-day d))
				  (set-date-dst?! d0 (date-dst? d))
				  (set-date-time-zone-offset! d0 (date-time-zone-offset d))))
	   d0)]
	[(arity-at-least)
	 (let ([a0 (make-arity-at-least #f)])
	   (vector-set! fixup n (lambda (a)
				  (set-arity-at-least-value! a0 (arity-at-least-value a))))
	   a0)])]))

  (define (deserialize l)
    (let ([mod-map (make-vector (list-ref l 0))]
	  [mod-map-l (list-ref l 1)]
	  [share-n (list-ref l 2)]
	  [shares (list-ref l 3)]
	  [fixups (list-ref l 4)]
	  [result (list-ref l 5)])
      ;; Load constructor mapping
      (let loop ([n 0][l mod-map-l])
	(unless (null? l)
	  (let* ([path+name (car l)]
		 [des (if (car path+name)
			  (dynamic-require (unprotect-path (car path+name))
					   (cdr path+name))
			  (namespace-variable-value (cdr path+name)))])
	    ;; Register maker and struct type:
	    (vector-set! mod-map n des))
	  (loop (add1 n) (cdr l))))
      ;; Create vector for sharing:
      (let ([share (make-vector share-n #f)]
	    [fixup (make-vector share-n #f)])
	;; Deserialize into sharing array:
	(let loop ([n 0][l shares])
	  (unless (= n share-n)
	    (vector-set! share n
			 (let ([v (car l)])
			   (if (box? v)
			       (deserial-shell (unbox v) mod-map fixup n)
			       (deserialize-one v share mod-map))))
	    (loop (add1 n) (cdr l))))
	;; Fixup shell for graphs
	(for-each (lambda (n+v)
		    (let ([v (deserialize-one (cdr n+v) share mod-map)])
		      ((vector-ref fixup (car n+v)) v)))
		  fixups)
	;; Deserialize final result. (If there's no sharing, then
	;;  all the work is actually here.)
	(deserialize-one result share mod-map)))))
