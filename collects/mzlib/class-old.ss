
;; Object system, same as MzScheme version 103 and earlier.

;; This was a fairly simple implementation until it was extended to
;;  handle primitive classes (e.g., for MrEd). Now it's a mess.

(module class-old mzscheme
  (require-for-syntax (lib "stx.ss""syntax")
		      (lib "name.ss""syntax"))

  (define insp (current-inspector))

  (define-values (prop:object object? object-ref) (make-struct-type-property 'object))

  (define-struct class (name
			object-make object-slot-ref object-slot-set!
			width method-prim-vec
			pos supers
			interface
			public-ht public-ids 
			init-old-indices    ;; rename
			init-define-indices ;; override, public
			replace-indices     ;; override
			init-new-indices    ;; inherit, override, public
			go go-arity
			struct:prim prop:dispatch prim-methods
			immediate-primitive?))
  ;; simplistic implementation for now:
  (define-struct interface (name supers public-ids class))

  (define undefined (letrec ([x x]) x))

  (define object<%> (make-interface 'object% null null #f))
  (define object% (make-class 'object%
			      'make-obj 'slot-ref 'slot-set! 0 (vector)
			      0 (vector #f) 
			      object<%>
			      (make-hash-table) 
			      null null null null null 
			      (lambda ()
				(let ([object%-init (lambda () (void))])
				  object%-init))
			      0
			      #f #f #f #f))
  (set-interface-class! object<%> object%)
  (vector-set! (class-supers object%) 0 object%)
  (define-values (struct:obj make-obj)
    (let-values ([(struct:obj make-obj obj? obj-accessor obj-mutator)
		  (make-struct-type 'object #f 0 0 #f (list (cons prop:object (box object%))) insp)])
      (set-class-object-make! object% make-obj)
      (set-class-object-slot-ref! object% obj-accessor)
      (set-class-object-slot-set!! object% obj-mutator)
      (values struct:obj make-obj)))

  (define (obj-class o)
    (unbox (object-ref o)))
  (define (slot-ref o p)
    (unbox/prim-resolve ((class-object-slot-ref (unbox (object-ref o))) o p) o))

  (define (make-naming-constructor type name)
    (let-values ([(struct: make- ? -accessor -mutator)
		  (make-struct-type name type 0 0 #f null insp)])
      make-))

  (define skip (gensym))

  (define-struct lazy-prim-method (m))

  (define (unbox/prim-resolve b o)
    (let ([v (unbox b)])
      (if (lazy-prim-method? v)
	  (begin
	    (set-box! b (lambda r (apply (lazy-prim-method-m v) o r)))
	    (unbox b))
	  v)))

  (define (make-prim-class struct:prim prim-side-box prop:dispatch prim-init name super old-mnames new-mnames methods)
    (let ([c (compose-class name (or super object%) null
			    null          ;; rename
			    null          ;; inherit
			    old-mnames    ;; override
			    new-mnames    ;; public
			    (let ([lazys
				   (map make-lazy-prim-method
					methods)])
			      (lambda (this super-init . pub-defines+pub-mutables)
				(lambda args
				  (if super (super-init skip) (super-init))
				  (unless (and (pair? args)
					       (eq? (car args) skip))
				    (apply prim-init this args))
				  (unless (null? pub-defines+pub-mutables)
				    ;; "define" all methods with lazy tokens:
				    (let loop ([ms lazys]
					       [l pub-defines+pub-mutables]) ; longer than ms
				      (unless (null? ms)
					(let ([m (car ms)])
					  (set-box! (car l) m))
					(loop (cdr ms) (cdr l))))))))
			    (box 0)
			    (list struct:prim prop:dispatch methods))])
      (set-box! prim-side-box c)
      c))

  (define-struct (exn:object exn) ())

  (define (obj-error where . msg)
    (raise
     (make-exn:object
      (string-append
       (format "~a: " where)
       (apply format msg))
      (current-continuation-marks))))

  (define (for-class name)
    (if name (format " for class: ~a" name) ""))
  (define (for-intf name)
    (if name (format " for interface: ~a" name) ""))

  (define (get-implement-requirement interfaces where for)
    (let loop ([class #f]
	       [supers interfaces])
      (if (null? supers)
	  class
	  (let ([c (interface-class (car supers))])
	    (loop
	     (cond
	      [(not c) class]
	      [(not class) c]
	      [(subclass? c class) class]
	      [(subclass? class c) c]
	      [else
	       (obj-error 
		where
		"conflicting class implementation requirements in superinterfaces~a"
		for)])
	     (cdr supers))))))

  (define (compose-class name super interfaces 
			 use-pre-ids   ;; rename
			 use-final-ids ;; inherit
			 replace-ids   ;; override
			 new-ids       ;; public
			 go go-arity
			 primitive)
    (unless (class? super)
      (obj-error 'class*/names "superclass expression returned a non-class: ~a~a" 
		 super
		 (for-class name)))
    (let ([name (or name
		    (let ([s (class-name super)])
		      (and s 
			   (not (eq? super object%))
			   (if (symbol? s)
			       (format "derived-from-~a" s)
			       s))))])
      (for-each
       (lambda (intf)
	 (unless (interface? intf)
	   (obj-error 'class*/names "interface expression returned a non-interface: ~a~a" 
		      intf
		      (for-class name))))
       interfaces)
      (let ([ht (make-hash-table)]
	    [super-public-ids (class-public-ids super)])
	;; Put superclass ids in table, with pos
	(let loop ([ids super-public-ids][p 0])
	  (unless (null? ids)
	    (hash-table-put! ht (car ids) p)
	    (loop (cdr ids) (add1 p))))
	;; Put new ids in table, with pos
	(let loop ([ids new-ids][p (class-width super)])
	  (unless (null? ids)
	    (when (hash-table-get ht (car ids) (lambda () #f))
	      (obj-error 'class*/names "superclass already contains ivar: ~a~a" 
			 (car ids)
			 (for-class name)))
	    (hash-table-put! ht (car ids) p)
	    (loop (cdr ids) (add1 p))))
	;; Check that superclass has expected ids, and get indices
	(let ([get-indices
	       (lambda (ids)
		 (map
		  (lambda (id)
		    (hash-table-get 
		     ht id
		     (lambda ()
		       (obj-error 'class*/names 
				  "superclass does not provide an expected ivar: ~a~a" 
				  id
				  (for-class name)))))
		  ids))]
	      [width (+ (class-width super) (length new-ids))]
	      [prop:dispatch (or (and primitive (cadr primitive))
				 (class-prop:dispatch super))]
	      [methods (and primitive 
			    (list->vector
			     (append (if (class-prim-methods super)
					 (vector->list (class-prim-methods super))
					 null)
				     (caddr primitive))))])
	  (let ([define-indices (get-indices (append replace-ids new-ids))]
		[use-pre-indices (get-indices use-pre-ids)]
		[use-final-indices (get-indices use-final-ids)]
		[replace-indices (get-indices replace-ids)]
		[new-indices (get-indices new-ids)]
		[method-prim-vec (make-vector width (and primitive #t))])

	    ;; Copy super's method prim flags:
	    (let ([svec (class-method-prim-vec super)])
	      (let loop ([i (class-width super)])
		(unless (zero? i)
		  (let ([i (sub1 i)])
		    (vector-set! method-prim-vec i (vector-ref svec i))
		    (loop i)))))
	    ;; If not prim, set prim-method flag for overridings
	    (unless primitive
	      (for-each (lambda (i)
			  (vector-set! method-prim-vec i #f))
			replace-indices))

	    ;; Check here that all interface ivars are satisfied
	    (for-each
	     (lambda (intf)
	       (for-each
		(lambda (var)
		  (unless (hash-table-get ht var (lambda () #f))
		    (obj-error 'class*/names 
			       "interface-required variable missing: ~a~a~a" 
			       var
			       (for-class name)
			       (for-intf (interface-name intf)))))
		(interface-public-ids intf)))
	     interfaces)
	    (let ([c (get-implement-requirement interfaces 'class*/names (for-class name))])
	      (when (and c (not (subclass? super c)))
		(obj-error 'class*/names 
			   "interface-required implementation not satisfied~a~a"
			   (for-class name)
			   (let ([r (class-name c)])
			     (if r
				 (format " required class: ~a" r)
				 "")))))
	    ;; Make the class and its interface
	    (let* ([class-make (if name
				   (make-naming-constructor 
				    struct:class 
				    (string->symbol (format "class:~a" name)))
				   make-class)]
		   [interface-make (if name
				       (make-naming-constructor 
					struct:interface
					(string->symbol (format "interface:~a" name)))
				       make-interface)]
		   [public-ids (append super-public-ids new-ids)]
		   [super-interfaces (cons (class-interface super) interfaces)]
		   [i (interface-make name super-interfaces public-ids #f)]
		   [struct:prim (or (and primitive (car primitive))
				    (class-struct:prim super))]
		   [c (class-make name
				  'object-make 'object-slot-ref 'object-slot-set!
				  width method-prim-vec
				  (add1 (class-pos super))
				  (list->vector (append (vector->list (class-supers super)) (list #f)))
				  i
				  ht public-ids
				  use-pre-indices ;; rename
				  define-indices  ;; override, public
				  replace-indices ;; override
				  (append use-final-indices replace-indices new-indices) ;; inherit, override, public
				  go (if (box? go-arity)
					 (make-arity-at-least (unbox go-arity))
					 go-arity)
				  struct:prim prop:dispatch methods
				  (and primitive #t))]
		   [obj-name (if name 
				 (string->symbol (format "object:~a" name))
				 'object)])
	      (let-values ([(struct:object object-make object-slot-ref object-slot-set!)
			    (let-values ([(t make p a m)
					  (make-struct-type obj-name
							    (or struct:prim #f)
							    0 ;; No init fields
							    ;; Unless prim, add uninit fields for slots:
							    (if primitive
								0
								width)
							    'uninitialized-slot ;; anything for uninit val
							    (append
							     (if (and struct:prim (not primitive))
								 ;; Need dispatcher
								 (list (cons prop:dispatch
									     (lambda (ivar-name)
									       (let ([pos (hash-table-get ht ivar-name)])
										 (if (vector-ref method-prim-vec pos)
										     #f
										     (lambda (o . args)
										       (apply (slot-ref o pos) args)))))))
								 null)
							     ;; Add/override backbox:
							     (list
							      (cons prop:object (box #f))))
							    insp)])
			      (values t
				      make
				      (if primitive
					  ;; No slots - selector always manufactures a value
					  (lambda (o p)
					    (box (lambda r (apply (vector-ref methods p) o r))))
					  ;; Normal object slots:
					  a)
				      m))])
		(set-box! (object-ref struct:object) c)
		(set-class-object-make! c object-make)
		(set-class-object-slot-ref! c object-slot-ref)
		(set-class-object-slot-set!! c object-slot-set!)
		(vector-set! (class-supers c) (class-pos c) c)
		(set-interface-class! i c)
		c)))))))

  (define (compose-interface name supers vars)
    (for-each
     (lambda (intf)
       (unless (interface? intf)
	 (obj-error 'interface 
		    "superinterface expression returned a non-interface: ~a~a" 
		    intf
		    (for-intf name))))
     supers)
    (let ([ht (make-hash-table)])
      (for-each
       (lambda (var)
	 (hash-table-put! ht var #t))
       vars)
      ;; Check that vars don't already exist in supers:
      (for-each
       (lambda (super)
	 (for-each
	  (lambda (var)
	    (when (hash-table-get ht var (lambda () #f))
	      (obj-error 'interface "variable already in superinterface: ~a~a~a" 
			 var
			 (for-intf name)
			 (let ([r (interface-name super)])
			   (if r
			       (format " already in: ~a" r)
			       "")))))
	  (interface-public-ids super)))
       supers)
      ;; Check for [conflicting] implementation requirements
      (let ([class (get-implement-requirement supers 'interface (for-intf name))]
	    [interface-make (if name
				(make-naming-constructor 
				 struct:interface
				 (string->symbol (format "interface:~a" name)))
				make-interface)])
	;; Add supervars to table:
	(for-each
	 (lambda (super)
	   (for-each
	    (lambda (var) (hash-table-put! ht var #t))
	    (interface-public-ids super)))
	 supers)
	;; Done
	(interface-make name supers (hash-table-map ht (lambda (k v) k)) class))))

  (define (make-object c . args)
    (unless (class? c)
      (apply raise-type-error 'make-object "class" 0 c args))
    (let ([this ((class-object-make c))])
      (let ([slot-ref (class-object-slot-ref c)]
	    [slot-set! (class-object-slot-set! c)]
	    [making-prim? (class-immediate-primitive? c)])
	(unless (class-immediate-primitive? c)
	  (let loop ([n (class-width c)])
	    (unless (= n 0)
	      (slot-set! this (sub1 n) (box undefined))
	      (loop (sub1 n)))))
	(let ([setup (let setup-class ([c c])
		       (if (zero? (class-pos c))
			   (lambda ()
			     ((class-go c)))
			   (let ([super (vector-ref (class-supers c) 
						    (sub1 (class-pos c)))])
			     (let ([super-setup (setup-class super)])
			       (let ([old-boxes (map (lambda (i)
						       (slot-ref this i))
						     (class-init-old-indices c))])
				 (for-each (lambda (i)
					     (slot-set! this i (box undefined)))
					   (if making-prim?
					       null
					       (class-replace-indices c)))
				 (let ([define-boxes (map (lambda (i)
							    (slot-ref this i))
							  (if making-prim?
							      null
							      (class-init-define-indices c)))])
				   (lambda ()
				     (let ([new-boxes (map (lambda (i)
							     (slot-ref this i))
							   (if making-prim?
							       null
							       (class-init-new-indices c)))]
					   [super-init (super-setup)]
					   [super-called? #f])
				       (letrec ([init (apply
						       (class-go c)
						       this
						       (lambda args
							 (when super-called?
							   (obj-error (or (object-name init)
									  'object-init)
								      "multiple intializations of superclass"))
							 (set! super-called? #t)
							 (apply super-init args)
							 ;; Force lazy method boxes that might be used directly:
							 (when (and (class-struct:prim c)
								    (not (class-immediate-primitive? c)))
							   (for-each (lambda (b) (unbox/prim-resolve b this)) old-boxes)
							   (for-each (lambda (b) (unbox/prim-resolve b this)) new-boxes)))
						       (append
							define-boxes  ;; override, public
							old-boxes     ;; rename
							new-boxes))]) ;; inherit, override, public
					 (lambda args
					   (apply init args)
					   (unless super-called?
					     (obj-error 
					      (or (object-name init)
						  'object-init)
					      "initialization did not invoke superclass initializer"))))))))))))])
	  (apply (setup) args)
	  this))))

  (define (is-a? v c)
    (cond
     [(class? c)
      (and (object? v)
	   (subclass? (obj-class v) c))]
     [(interface? c)
      (and (object? v)
	   (implementation? (obj-class v) c))]
     [else (raise-type-error 'is-a? "class or interface" 1 v c)]))

  (define (subclass? v c)
    (unless (class? c)
      (raise-type-error 'subclass? "class" 1 v c))
    (and (class? v)
	 (let ([p (class-pos c)])
	   (and (<= p (class-pos v))
		(eq? c (vector-ref (class-supers v) p))))))

  (define class->interface class-interface)
  (define (object-interface o) (class-interface (obj-class o)))

  (define (implementation? v i)
    (unless (interface? i)
      (raise-type-error 'implementation? "interface" 1 v i))
    (and (class? v)
	 (interface-extension? (class->interface v) i)))

  (define (interface-extension? v i)
    (unless (interface? i)
      (raise-type-error 'interface-extension? "interface" 1 v i))
    (and (interface? i)
	 (let loop ([v v])
	   (or (eq? v i)
	       (ormap loop (interface-supers v))))))

  (define (ivar-in-interface? s i)
    (unless (symbol? s)
      (raise-type-error 'ivar-in-interface? "symbol" 0 s i))
    (unless (interface? i)
      (raise-type-error 'ivar-in-interface? "interface" 1 s i))
    (and (memq s (interface-public-ids i)) #t))

  (define (interface->ivar-names i)
    (unless (interface? i)
      (raise-type-error 'interface->ivar-names "interface" i))
					; copy list
    (map values (interface-public-ids i)))

  (define (class-initialization-arity c)
    (unless (class? c)
      (raise-type-error 'class-initialization-arity "class" c))
    (class-go-arity c))

  (define (ivar/proc o n)
    (unless (object? o)
      (raise-type-error 'ivar/proc "object" 0 o n))
    (let ([c (obj-class o)])
      (let ([p (hash-table-get
		(class-public-ht c)
		n
		(lambda () #f))])
	(if p
	    (slot-ref o p)
	    (begin
	       (unless (symbol? n)
		 (raise-type-error 'ivar/proc "symbol" 1 o n))
	       (obj-error 'ivar 
			  "instance variable not found: ~e~a in: ~e" 
			  n 
			  (for-class (class-name c))
			  o))))))
  
  (define-syntax ivar
    (lambda (stx)
      (syntax-case stx ()
	[(_ o m)
	 (identifier? (syntax m))
	 (syntax (ivar/proc o 'm))])))

  (define-syntax send
    (lambda (stx)
      (syntax-case stx ()
	[(_ o m arg ...)
	 (identifier? (syntax m))
	 (syntax ((ivar o m) arg ...))])))

  (define-syntax send*
    (lambda (stx)
      (syntax-case stx ()
	[(_ obj (meth arg ...) ...)
	 (syntax/loc stx
		     (let ([o obj])
		       (send o meth arg ...)
		       ...))])))
  
  (define (make-generic/proc c n)
    (unless (or (class? c) (interface? c))
      (raise-type-error 'make-generic "class or interface" 0 c n))
    (unless (symbol? n)
      (raise-type-error 'make-generic/proc "symbol" 1 c n))
    (if (class? c)
	(let ([p (hash-table-get
		  (class-public-ht c)
		  n
		  (lambda () #f))])
	  (if p
	      (lambda (o)
		(unless (is-a? o c)
		  (let ([name (string->symbol (format "generic~a" (for-class (class-name c))))])
		    (if (object? o)
			(obj-error name
				   "object not an instance of the generic's class: ~e"
				   o)
			(raise-type-error
			 name
			 "object"
			 o))))
		(slot-ref o p))
	      (obj-error 'make-generic 
			 "instance variable not found: ~e~a" 
			 n 
			 (for-class (class-name c)))))
	(begin
	  (unless (memq n (interface-public-ids c))
	    (obj-error 'make-generic 
		       "instance variable not found: ~e~a" 
		       n 
		       (for-intf (interface-name c))))
	  (lambda (o)
	    (unless (is-a? o c)
	      (let ([name (string->symbol (format "generic~a" (for-intf (interface-name c))))])
		(if (object? o)
		    (obj-error name
			       "object not an instance of the generic's interface: ~e"
			       o)
		    (raise-type-error
		     name
		     "object"
		     o))))
	    (ivar/proc o n)))))

  (define-syntax make-generic
    (lambda (stx)
      (syntax-case stx ()
	[(_ c n)
	 (identifier? (syntax n))
	 (syntax
	  (make-generic/proc c 'n))])))

  (define needs-init (gensym))

  (define-syntax class*/names
    (lambda (stx)
      (syntax-case stx ()
	[(_ (this-id super-init-id)
	    super-expr
	    (interface-expr ...)
	    init-vars
	    clauses ...)
	 (let ([se (lambda (msg expr)
		     (raise-syntax-error #f msg stx expr))])
	   ;; Check this and super-init:
	   (unless (identifier? (syntax this-id))
	     (se "not an identifier" (syntax this-id)))
	   (unless (identifier? (syntax super-init-id))
	     (se "not an identifier" (syntax super-init-id)))
	   ;; Unpack init arguments, with default expressions:
	   (let-values ([(init-ids init-defs init-rest-id)
			 (let loop ([inits (syntax init-vars)][need-def? #f])
			   (syntax-case inits ()
			     [() (values null null #f)]
			     [id (identifier? (syntax id))
				 (values null null (syntax id))]
			     [(id . rest) (identifier? (syntax id))
			      (begin
				(when need-def?
				  (se "expected identifier with default value" (syntax id)))
				(let-values ([(ids defs rest) (loop (syntax rest) #f)])
				  (values (cons (syntax id) ids)
					  (cons #f defs)
					  rest)))]
			     [((id def) . rest) (identifier? (syntax id))
			      (let-values ([(ids defs rest) (loop (syntax rest) #f)])
				(values (cons (syntax id) ids)
					(cons (syntax def) defs)
					rest))]
			     [(first . rest)
			      (se "bad initialization declaration" (syntax first))]
			     [else (se "improper identifier list" (syntax init-vars))]))])
	     ;; Unpack all body clauses:
	     (let* ([extract-ivars
		     ;; Unpacks a public, private, or override clause
		     (lambda (kind can-rename? decls)
		       (map
			(lambda (decl)
			  (syntax-case decl ()
			    [id (identifier? (syntax id))
				(list kind (syntax id) (syntax id) (syntax (void)))]
			    [(id expr) (identifier? (syntax id))
			     (list kind (syntax id) (syntax id) (syntax expr))]
			    [(id) (and can-rename? (identifier? (syntax id)))
			     (list kind (syntax id) (syntax id) (syntax (void)))]
			    [((iid eid) expr) (and can-rename?
						   (identifier? (syntax iid))
						   (identifier? (syntax eid)))
			     (list kind (syntax iid) (syntax eid) (syntax expr))]
			    [else (se (format "bad ~a clause" kind) (syntax decl))]))
			(syntax->list decls)))]
		    [body 
		     ;; Make a list of normalized clause-like lists, e.g:
		     ;;  (list (list 'public internal-id extenal-id expr) ...)
		     (apply
		      append
		      (map
		       (lambda (clause)
			 (syntax-case clause (public override private rename inherit sequence)
			   [(public decl ...)
			    (extract-ivars 'public #t (syntax (decl ...)))]
			   [(override decl ...)
			    (extract-ivars 'override #t (syntax (decl ...)))]
			   [(private decl ...)
			    (extract-ivars 'private #f (syntax (decl ...)))]
			   [(rename (iid eid) ...)
			    (let ([iids (syntax->list (syntax (iid ...)))]
				  [eids (syntax->list (syntax (eid ...)))])
			      (for-each (lambda (s)
					  (unless (identifier? s)
					    (se "expected an identifier" s)))
					(append iids eids))
			      (map (lambda (iid eid)
				     (list 'rename iid eid))
				   iids eids))]
			   [(inherit id ...)
			    (map
			     (lambda (decl)
			       (syntax-case decl ()
				 [id (identifier? (syntax id))
				     (list 'inherit (syntax id) (syntax id))]
				 [(iid eid) (and (identifier? (syntax iid))
						 (identifier? (syntax eid)))
				  (list 'inherit (syntax iid) (syntax eid))]
				 [else (se "bad inherit clause" decl)]))
			     (syntax->list (syntax (id ...))))]
			   [(sequence expr ...)
			    (map
			     (lambda (expr)
			       (list 'sequence expr))
			     (syntax->list (syntax (expr ...))))]
			   [else (se "not a class clause" clause)]))
		       (syntax->list (syntax (clauses ...)))))]
		    [get-info (lambda (tags select)
				(let loop ([body body])
				  (cond
				   [(null? body) null]
				   [(memq (caar body) tags)
				    (cons (select (car body)) (loop (cdr body)))]
				   [else (loop (cdr body))])))])
	       ;; Extract internal and external ids, and create xformed body:
	       (let ([new-eids (get-info '(public) caddr)]
		     [use-pre-eids (get-info '(rename) caddr)]
		     [use-final-eids (get-info '(inherit) caddr)]
		     [replace-eids (get-info '(override) caddr)]
		     [inherited-ids (get-info '(inherit rename) cadr)]
		     [public-ids (get-info '(public override) cadr)]
		     [private-ids (get-info '(private) cadr)]
		     [immutable-boxed-ids (append (get-info '(rename) cadr) ;; order matters!
						  (get-info '(inherit) cadr))]
		     [mutable-boxed-ids (append (get-info '(override) cadr) ;; order matters!
						(get-info '(public) cadr))])
		 (let* ([define-eids (append replace-eids new-eids)]
			[define-iids (generate-temporaries define-eids)]
			[body-exprs 
			 (map
			  ;; Map each declaration clause to a set!:
			  (lambda (clause)
			    (if (eq? (car clause) 'sequence)
				(cadr clause)
				(with-syntax ([id (cadr clause)]
					      [expr (cadddr clause)])
				  (if (memq (car clause) '(public override))
				      (let ([eid (caddr clause)])
					(with-syntax ([nid (let loop ([de define-eids]
								      [di define-iids])
							     (if (eq? eid (car de))
								 (car di)
								 (loop (cdr de) (cdr di))))])
					  ;; let sets name for expr:
					  (syntax (set-box! nid (let ([id expr]) id)))))
				      (syntax (set! id expr))))))
			  (get-info '(public override private sequence) values))]
			[name (syntax-local-infer-name stx)])
		   ;; Check for duplicates:
		   (cond
		    [(check-duplicate-identifier 
		      (append new-eids use-final-eids replace-eids))
		     => (lambda (name)
			  (se "duplicate declaration of external name" name))]
		    [(check-duplicate-identifier 
		      (append init-ids (if init-rest-id (list init-rest-id) null)
			      public-ids inherited-ids private-ids))
		     => (lambda (name)
			  (se "duplicate declaration of identifier" name))]
		    [else (void)])
		   ;; ---------- build the result ----------
		   ;; References to non-private ivars are converted to box
		   ;; references.
		   (with-syntax ([use-pre-eids use-pre-eids]
				 [use-final-eids use-final-eids]
				 [replace-eids replace-eids]
				 [new-eids new-eids]
				 [define-eids define-eids]
				 [(define-iid ...) define-iids]
				 [(immutable-box-id ...) (generate-temporaries immutable-boxed-ids)]
				 [(mutable-box-id ...) (generate-temporaries mutable-boxed-ids)]
				 [(immutable-boxed-id ...) immutable-boxed-ids]
				 [(mutable-boxed-id ...) mutable-boxed-ids]
				 [(private-id ...) private-ids]
				 [body-exprs (if (null? body-exprs)
						 (syntax ((void)))
						 body-exprs)]
				 [init (datum->syntax-object
					#f
					(if name
					    (string->symbol (format "~a-init" name))
					    'init)
					#f)]
				 [name (datum->syntax-object #f name #f)])
		     (with-syntax ([go 
				    ;; Create a sequence of case-lambda
				    ;; clauses, to implement init variable defaults:
				    (let loop ([vars-so-far null]
					       [vars-w/def-so-far null]
					       [def-so-far null]
					       [init-ids init-ids]
					       [init-defs init-defs])
				      (cond
				       [(null? init-ids)
					(with-syntax ([(var ...) (reverse vars-so-far)]
						      [(maybe-inited ...) (generate-temporaries vars-w/def-so-far)]
						      [(dvar ...) (reverse vars-w/def-so-far)]
						      [(def ...) (reverse def-so-far)]
						      [rest-id (if init-rest-id
								   init-rest-id
								   null)])
					  (syntax
					   ([(var ... maybe-inited ... . rest-id)
					     (let ([dvar undefined] ...)
					       (set! dvar (if (eq? maybe-inited needs-init)
							      def
							      maybe-inited)) ...
							      .
							      body-exprs)])))]
				       [else
					(with-syntax ([rest (loop (if (car init-defs)
								      vars-so-far
								      (cons (car init-ids) vars-so-far))
								  (if (car init-defs)
								      (cons (car init-ids) vars-w/def-so-far)
								      null)
								  (if (car init-defs)
								      (cons (car init-defs) def-so-far)
								      null)
								  (cdr init-ids)
								  (cdr init-defs))])
					  (if (car init-defs)
					      (with-syntax ([(var ...) (reverse vars-so-far)]
							    [(wd-var ...) (reverse vars-w/def-so-far)])
						(syntax ([(var ... wd-var ...) (init var ... wd-var ... needs-init)]
							 . rest)))
					      (syntax rest)))]))]
				   [go-arity (datum->syntax-object
					      #f
					      (let ([req (let loop ([l init-defs][c 0])
							   (if (or (null? l) (car l))
							       c
							       (loop (cdr l) (add1 c))))]
						    [cnt (length init-ids)])
						(cond
						 [init-rest-id
						  (box req)]
						 [(< req cnt)
						  (let loop ([req req])
						    (if (= req cnt)
							(list req)
							(cons req (loop (add1 req)))))]
						 [else req]))
					      #f)])
		       ;; Assemble the result as a `compose-class-info' call,
		       ;; which does all the run-time checks, and knows how
		       ;; to allocate objects and pass boxes to the init
		       ;; function.
		       (syntax/loc
			stx
			(compose-class
			 'name
			 super-expr
			 (list interface-expr ...)
			 'use-pre-eids   ;; rename
			 'use-final-eids ;; inherit
			 'replace-eids   ;; override
			 'new-eids       ;; public
			 (lambda (this-id super-init-id 
					  define-iid ...        ;; override, public
					  immutable-box-id ...  ;; rename, inherit
					  mutable-box-id ...)   ;; override, public
			   (let-syntax ([immutable-boxed-id
					 (make-set!-transformer
					  (lambda (stx)
					    (syntax-case stx (set!)
					      [vr (identifier? (syntax vr)) 
						  (syntax (unbox immutable-box-id))]
					      [(set! vr val)
					       (raise-syntax-error
						#f
						"cannot mutate an inherit or rename variable"
						stx)]
					      [(vr . args) (syntax ((unbox immutable-box-id) . args))])))]
					...
					[mutable-boxed-id
					 (make-set!-transformer
					  (lambda (stx)
					    (syntax-case stx (set!)
					      [vr (identifier? (syntax vr)) 
						  (syntax (unbox mutable-box-id))]
					      [(set! vr val)
					       (syntax 
						(set-box! mutable-box-id 
							  ;; let vr gives val the right name
							  (let ([vr val]) vr)))]
					      [(vr . args) (syntax ((unbox mutable-box-id) . args))])))]
					...)
			     (let ([private-id undefined] ...)
			       (letrec ([init (case-lambda . go)])
				 init))))
			 'go-arity
			 #f)))))))))]
	;; Error cases
	;; --
	[(_ bad-this-super
	    super-expr
	    (interface-expr ...)
	    init-vars
	    clauses ...)
	 (raise-syntax-error 
	  #f
	  "bad this and super bindings"
	  stx
	  (syntax bad-this-super))]
	;; --
	[(_ this-super
	    super-expr
	    bad-interface-seq
	    init-vars
	    clauses ...)
	 (raise-syntax-error 
	  #f
	  "expected sequence of interface expressions"
	  stx
	  (syntax bad-interface-seq))]
	;;
	[(_ this-super
	    super-expr
	    interface-seq)
	 (raise-syntax-error 
	  #f
	  "missing initialization arguments"
	  stx
	  (syntax bad-this-super))]
	[(_ this-super
	    super-expr)
	 (raise-syntax-error 
	  #f
	  "missing interface expressions"
	  stx
	  (syntax bad-this-super))]
	[(_ this-super)
	 (raise-syntax-error 
	  #f
	  "missing this and super-init bindings"
	  stx
	  (syntax bad-this-super))])))

  (define-syntax class*
    (lambda (stx)
      (syntax-case stx ()
	[(_ super-expr
	    (interface-expr ...)
	    init-vars
	    clauses ...)
	 (with-syntax ([this (datum->syntax-object (stx-car stx) 'this stx)]
		       [super-init (datum->syntax-object (stx-car stx) 'super-init stx)])
	   (syntax/loc
	    stx
	    (class*/names (this super-init)
			  super-expr 
			  (interface-expr ...)
			  init-vars
			  clauses ...)))])))

  (define-syntax :class
    (lambda (stx)
      (syntax-case stx ()
	[(_ super-expr
	    init-vars
	    clauses ...)
	 (with-syntax ([class* (datum->syntax-object (stx-car stx) 'class* stx)])
	   (syntax/loc stx (class* super-expr () init-vars clauses ...)))])))

  (define-syntax class*-asi
    (lambda (stx)
      (syntax-case stx ()
	[(_ super (interface ...) body ...)
	 (syntax/loc stx (class* super (interface ...) args
			   body ...))])))

  (define-syntax class-asi
    (lambda (stx)
      (syntax-case stx ()
	[(_ super body ...)
	 (syntax/loc stx (class* super () args
			   body ...))])))
  
  (define-syntax :interface
    (lambda (stx)
      (syntax-case stx ()
	[(_ (interface-expr ...) var ...)
	 (let ([vars (syntax->list (syntax (var ...)))]
	       [name (syntax-local-infer-name stx)])
	   (for-each
	    (lambda (v)
	      (unless (identifier? v)
		(raise-syntax-error #f
				    "not an identifier"
				    stx
				    v)))
	    vars)
	   (let ([dup (check-duplicate-identifier vars)])
	     (when dup
	       (raise-syntax-error #f
				   "duplicate name"
				   stx
				   dup)))
	   (with-syntax ([name (datum->syntax-object #f name #f)])
	     (syntax/loc
	      stx
	      (compose-interface
	       'name
	       (list interface-expr ...)
	       '(var ...)))))])))

  (provide (rename :class class) class* class*/names
	   class-asi class*-asi
	   (rename :interface interface)
	   make-object object? is-a? subclass? class? interface?
	   class->interface object-interface
	   implementation? interface-extension?
	   ivar-in-interface? interface->ivar-names
	   class-initialization-arity
	   ivar send send* make-generic
	   ivar/proc make-generic/proc
	   object% ;; object<%>
	   exn:object? struct:exn:object make-exn:object

	   prop:object make-prim-class))
