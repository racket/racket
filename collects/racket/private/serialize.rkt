(module serialize racket/base
  (require syntax/modcollapse
           unstable/struct
           racket/list
           racket/flonum
           racket/fixnum
           "serialize-structs.rkt")

  ;; This module implements the core serializer. The syntactic
  ;; `define-serializable-struct' layer is implemented separately
  ;; (and differently for old-style vs. new-style `define-struct').

  (provide prop:serializable
	   make-serialize-info
	   make-deserialize-info

	   ;; Checks whether a value is serializable:
	   serializable?

	   ;; The two main routines:
	   serialize
	   deserialize

           serialized=?

           deserialize-module-guard)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; serialize
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (serializable? v)
    (or (serializable-struct? v)
        (and (struct? v)
             (prefab-struct-key v)
             #t)
	(boolean? v)
	(null? v)
	(number? v)
	(char? v)
	(and (symbol? v)
             (or (symbol-interned? v)
                 (eq? v (string->unreadable-symbol (symbol->string v)))))
	(string? v)
	(path-for-some-system? v)
	(bytes? v)
	(vector? v)
	(flvector? v)
	(fxvector? v)
	(pair? v)
	(mpair? v)
	(hash? v)
	(box? v)
	(void? v)
	(date? v)
	(arity-at-least? v)
        (module-path-index? v)
        (srcloc? v)))

  ;; If a module is dynamic-required through a path,
  ;;  then it can cause simplified module paths to be paths;
  ;;  keep the literal path, but marshal it to bytes.
  (define (protect-path p)
    (cond
     [(path? p) (path->bytes p)]
     [(and (pair? p) (eq? (car p) 'submod) (path? (cadr p)))
      `(submod ,(protect-path (cadr p)) . ,(cddr p))]
     [else p]))
  (define (unprotect-path p)
    (cond
     [(bytes? p) (bytes->path p)]
     [(and (pair? p) (eq? (car p) 'submod) (bytes? (cadr p)))
      `(submod ,(unprotect-path (cadr p)) . ,(cddr p))]
     [else p]))

  (define (revive-symbol s)
    (if (string? s)
        (string->unreadable-symbol s)
        s))

  (define deserialize-module-guard (make-parameter (lambda (mod-path sym) 
                                                     (void))))
  (define varref (#%variable-reference varref))

  (define (collapse/resolve-module-path-index mpi rel-to)
    (let ([v (collapse-module-path-index mpi rel-to)])
      (if (path? v)
          ;; If collapsing gives a path, then we can't do any better than
          ;; resolving --- and we must resolved, because the mpi may record
          ;; a more accurate path inside.
          (let ([v2 (resolved-module-path-name (module-path-index-resolve mpi))])
            (if (symbol? v2)
                `(quote ,v2)
                v2))
          v)))
  
  (define (mod-to-id info mod-map cache)
    (let ([deserialize-id (serialize-info-deserialize-id info)])
      (hash-ref 
       cache deserialize-id
       (lambda ()
	 (let ([id
		(let ([path+name
		       (cond
			[(identifier? deserialize-id)
			 (let ([b (identifier-binding deserialize-id (variable-reference->phase varref))])
			   (cons
			    (and (list? b)
				 (if (symbol? (caddr b))
				     (caddr b)
				     (protect-path
				      (collapse/resolve-module-path-index 
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
			       (collapse/resolve-module-path-index 
				(cdr deserialize-id)
				(build-path (serialize-info-dir info)
					    "here.ss"))))
			  (car deserialize-id))])])
		  (hash-ref 
		   mod-map path+name
		   (lambda ()
		     (let ([id (hash-count mod-map)])
		       (hash-set! mod-map path+name id)
		       id))))])
	   (hash-set! cache deserialize-id id)
	   id)))))

  (define (is-mutable? o)
    (or (and (or (mpair? o)
		 (box? o)
		 (vector? o)
		 (hash? o))
	     (not (immutable? o)))
	(serializable-struct? o)
        (flvector? o)
        (fxvector? o)
        (let ([k (prefab-struct-key o)])
          (and k
               ;; Check whether all fields are mutable:
               (pair? k)
               (let-values ([(si skipped?) (struct-info o)])
                 (let loop ([si si])
                   (let*-values ([(name init auto acc mut imms super skipped?) (struct-type-info si)])
                     (and (null? imms)
                          (or (not super)
                              (loop super))))))))))

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
    (+ (hash-count share)
       (hash-count cycle)))

  ;; Traverses v to find cycles and sharing. Shared
  ;;  objects go in the `shared' table, and cycle-breakers go in
  ;;  `cycle'. In each case, the object is mapped to a number that is
  ;;  incremented as shared/cycle objects are discovered, so
  ;;  when the objects are deserialized, build them in reverse
  ;;  order.
  (define (find-cycles-and-sharing v cycle share)
    (let ([tmp-cycle (make-hasheq)]  ;; candidates for sharing
	  [tmp-share (make-hasheq)]  ;; candidates for cycles
	  [cycle-stack null])      ;; same as in tmpcycle, but for finding mutable
      (let loop ([v v])
	(cond
	 [(or (boolean? v)
	      (number? v)
	      (char? v)
	      (symbol? v)
	      (null? v)
	      (void? v)
              (srcloc? v))
	  (void)]
	 [(hash-ref cycle v #f)
	  ;; We already know that this value is
	  ;;  part of a cycle
	  (void)]
	 [(hash-ref tmp-cycle v #f)
	  ;; We've just learned that this value is
	  ;;  part of a cycle.
	  (let ([mut-v (if (is-mutable? v)
			   v
			   (find-mutable v cycle-stack))])
	    (hash-set! cycle mut-v (share-id share cycle))
	    (unless (eq? mut-v v)
	      ;; This value is potentially shared
	      (hash-set! share v (share-id share cycle))))]
	 [(hash-ref share v #f)
	  ;; We already know that this value is shared
	  (void)]
	 [(hash-ref tmp-share v #f)
	  ;; We've just learned that this value is
	  ;;  shared
	  (hash-set! share v (share-id share cycle))]
	 [else
	  (hash-set! tmp-share v #t)
	  (hash-set! tmp-cycle v #t)
	  (set! cycle-stack (cons v cycle-stack))
	  (cond
	   [(serializable-struct? v)
	    (let ([info (serializable-info v)])
	      (for-each loop (vector->list ((serialize-info-vectorizer info) v))))]
           [(and (struct? v)
                 (prefab-struct-key v))
            (for-each loop (struct->list v))]
	   [(or (string? v)
		(bytes? v)
		(path-for-some-system? v))
	    ;; No sub-structure
	    (void)]
	   [(vector? v)
	    (for-each loop (vector->list v))]
           [(flvector? v) (void)] 
           [(fxvector? v) (void)] 
	   [(pair? v)
	    (loop (car v)) 
	    (loop (cdr v))]
	   [(mpair? v)
	    (loop (mcar v)) 
	    (loop (mcdr v))]
	   [(box? v)
	    (loop (unbox v))]
	   [(date*? v)
	    (for-each loop (take (struct->list v) 12))]
	   [(date? v)
	    (for-each loop (take (struct->list v) 10))]
	   [(hash? v)
	    (hash-for-each v (lambda (k v)
                               (loop k)
                               (loop v)))]
	   [(arity-at-least? v)
	    (loop (arity-at-least-value v))]
	   [(module-path-index? v)
            (let-values ([(path base) (module-path-index-split v)])
              (loop path)
              (loop base))]
	   [else (raise-argument-error
		  'serialize
		  "serializable?"
		  v)])
	  ;; No more possibility for this object in
	  ;;  a cycle:
	  (hash-remove! tmp-cycle v)
	  (set! cycle-stack (cdr cycle-stack))]))))

  (define (quotable? v)
    (if (pair? v)
        (eq? (car v) 'q)
        (or (boolean? v)
            (number? v)
            (char? v)
            (null? v)
            (string? v)
            (symbol? v)
            (bytes? v))))

  (define (serialize-one v share check-share? mod-map mod-map-cache)
    (define ((serial check-share?) v)
      (cond
       [(or (boolean? v)
	    (number? v)
	    (char? v)
	    (null? v))
	v]
       [(symbol? v)
        (if (symbol-interned? v)
            v
            (cons 'su (symbol->string v)))]
       [(void? v)
	'(void)]
       [(and check-share?
	     (hash-ref share v #f))
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
       [(and (struct? v)
             (prefab-struct-key v))
        => (lambda (k)
             (cons 'f
                   (cons
                    k
                    (map (serial #t) (struct->list v)))))]
       [(or (string? v)
	    (bytes? v))
	(cons 'u v)]
       [(path-for-some-system? v)
	(list* 'p+ (path->bytes v) (path-convention-type v))]
       [(vector? v)
        (define elems (map (serial #t) (vector->list v)))
        (if (and (immutable? v)
                 (andmap quotable? elems))
            (cons 'q v)
            (cons (if (immutable? v) 'v 'v!) elems))]
       [(flvector? v)
        (cons 'vl (for/list ([i (in-flvector v)]) i))]
       [(fxvector? v)
        (cons 'vx (for/list ([i (in-fxvector v)]) i))]
       [(pair? v)
	(let ([loop (serial #t)])
          (let ([a (loop (car v))]
                [d (loop (cdr v))])
            (cond
             [(and (quotable? a) (quotable? d))
              (cons 'q v)]
             [else
              (cons 'c (cons a d))])))]
       [(mpair? v)
	(let ([loop (serial #t)])
	  (cons 'm
		(cons (loop (mcar v)) 
		      (loop (mcdr v)))))]
       [(box? v)
	(cons (if (immutable? v) 'b 'b!)
	      ((serial #t) (unbox v)))]
       [(hash? v)
	(list* 'h
	       (if (immutable? v) '- '!)
	       (append
		(if (hash-equal? v) '(equal) null)
		(if (hash-eqv? v) '(eqv) null)
		(if (hash-weak? v) '(weak) null))
	       (let ([loop (serial #t)])
		 (hash-map v (lambda (k v)
                               (cons (loop k)
                                     (loop v))))))]
       [(date*? v)
	(cons 'date*
	      (map (serial #t) (take (struct->list v) 12)))]
       [(date? v)
	(cons 'date
	      (map (serial #t) (take (struct->list v) 10)))]
       [(arity-at-least? v)
	(cons 'arity-at-least
	      ((serial #t) (arity-at-least-value v)))]
       [(module-path-index? v)
        (let-values ([(path base) (module-path-index-split v)])
          (cons 'mpi
                (cons ((serial #t) path)
                      ((serial #t) base))))]
       [(srcloc? v)
        (cons 'srcloc
              (map (serial #t) (take (struct->list v) 5)))]
       [else (error 'serialize "shouldn't get here")]))
    ((serial check-share?) v))
  
  (define (serial-shell v mod-map mod-map-cache)
    (cond
     [(serializable-struct? v)
      (let ([info (serializable-info v)])
	(mod-to-id info mod-map mod-map-cache))]
     [(vector? v)
      (cons 'v (vector-length v))]
     [(mpair? v)
      'm]
     [(box? v)
      'b]
     [(hash? v)
      (cons 'h (append
		(if (hash-equal? v) '(equal) null)
		(if (hash-eqv? v) '(eqv) null)
		(if (hash-weak? v) '(weak) null)))]
     [else
      ;; A mutable prefab
      (cons 'pf (cons (prefab-struct-key v)
                      (sub1 (vector-length (struct->vector v)))))]))

  (define (serialize v)
    (let ([mod-map (make-hasheq)]
	  [mod-map-cache (make-hash)]
	  [share (make-hasheq)]
	  [cycle (make-hasheq)])
      ;; First, traverse V to find cycles and sharing
      (find-cycles-and-sharing v cycle share)
      ;; To simplify, all add the cycle records to shared.
      ;;  (but keep cycle info, too).
      (hash-for-each cycle
                     (lambda (k v)
                       (hash-set! share k v)))
      (let ([ordered (map car (sort (hash-map share cons)
                                    (lambda (a b) (< (cdr a) (cdr b)))))])
	(let ([serializeds (map (lambda (v)
				  (if (hash-ref cycle v #f)
				      ;; Box indicates cycle record allocation
				      ;;  followed by normal serialization
				      (box (serial-shell v mod-map mod-map-cache))
				      ;; Otherwise, normal serialization
				      (serialize-one v share #f mod-map mod-map-cache)))
				ordered)]
	      [fixups (hash-map 
		       cycle
		       (lambda (v n)
			 (cons n
			       (serialize-one v share #f mod-map mod-map-cache))))]
	      [main-serialized (serialize-one v share #t mod-map mod-map-cache)]
	      [mod-map-l (map car (sort (hash-map mod-map cons)
                                        (lambda (a b) (< (cdr a) (cdr b)))))])
	  (list '(3) ;; serialization-format version
                (hash-count mod-map)
		(map (lambda (v) (if (symbol-interned? (cdr v))
                                     v 
                                     (cons (car v) (symbol->string (cdr v)))))
                     mod-map-l)
		(length serializeds)
		serializeds
		fixups
		main-serialized)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; deserialize
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (make-hash/flags v)
    (if (null? v) 
        (make-hasheq)
        (case (car v)
          [(equal)
           (if (null? (cdr v))
               (make-hash)
               (make-weak-hash))]
          [(eqv)
           (if (null? (cdr v))
               (make-hasheqv)
               (make-weak-hasheqv))]
          [(weak)
           (make-weak-hasheq)])))

  (define-struct not-ready (shares fixup))

  (define (lookup-shared! share n mod-map module-path-index-join)
    ;; The shared list is not necessarily in order of
    ;;  referreds before referees. A `not-ready' object
    ;;  indicates a reference before a value is ready,
    ;;  so we need to recur to make it ready. Cycles
    ;;  have been broken, though, so we don't run into
    ;;  trouble with an infinite loop here.
    (let ([sv (vector-ref share n)])
      (if (not-ready? sv)
          (let* ([v (vector-ref (not-ready-shares sv) n)]
                 [val (if (box? v)
                          (deserial-shell (unbox v) mod-map (not-ready-fixup sv) n)
                          (deserialize-one v share mod-map module-path-index-join))])
            (vector-set! share n val)
            val)
          sv)))

  (define (deserialize-one v share mod-map module-path-index-join)
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
	  [(?) (lookup-shared! share (cdr v) mod-map module-path-index-join)]
          [(q) (cdr v)]
          [(f) (apply make-prefab-struct (cadr v) (map loop (cddr v)))]
	  [(void) (void)]
          [(su) (string->unreadable-symbol (cdr v))]
	  [(u) (let ([x (cdr v)])
		 (cond
		  [(string? x) (string-copy x)]
		  [(bytes? x) (bytes-copy x)]))]
	  [(p) (bytes->path (cdr v))]
	  [(p+) (bytes->path (cadr v) (cddr v))]
	  [(c) (cons (loop (cadr v)) (loop (cddr v)))]
	  [(c!) (cons (loop (cadr v)) (loop (cddr v)))]
	  [(m) (mcons (loop (cadr v)) (loop (cddr v)))]
	  [(v) (apply vector-immutable (map loop (cdr v)))]
	  [(v!) (list->vector (map loop (cdr v)))]
          [(vl) (apply flvector (map loop (cdr v)))]
          [(vx) (apply fxvector (map loop (cdr v)))]
	  [(b) (box-immutable (loop (cdr v)))]
	  [(b!) (box (loop (cdr v)))]
	  [(h) (let ([al (map (lambda (p)
				(cons (loop (car p))
				      (loop (cdr p))))
			      (cdddr v))])
		 (if (eq? '! (cadr v))
		     (let ([ht (make-hash/flags (caddr v))])
		       (for-each (lambda (p)
				   (hash-set! ht (car p) (cdr p)))
				 al)
		       ht)
                     (if (null? (caddr v))
                         (make-immutable-hasheq al)
                         (if (eq? (caaddr v) 'equal)
                             (make-immutable-hash al)
                             (make-immutable-hasheqv al)))))]
	  [(date) (apply make-date (map loop (cdr v)))]
	  [(date*) (apply make-date* (map loop (cdr v)))]
	  [(arity-at-least) (make-arity-at-least (loop (cdr v)))]
	  [(mpi) (module-path-index-join (loop (cadr v))
                                         (loop (cddr v)))]
          [(srcloc) (apply make-srcloc (map loop (cdr v)))]
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
	 (let ([ht0 (make-hash/flags (cdr v))])
	   (vector-set! fixup n (lambda (ht)
				  (hash-for-each 
				   ht
				   (lambda (k v)
				     (hash-set! ht0 k v)))))
	   ht0)]
        [(pf)
         ;; Prefab
         (let ([s (apply make-prefab-struct 
                         (cadr v)
                         (vector->list (make-vector (cddr v) #f)))])
           (vector-set! fixup n (lambda (v)
                                  (let-values ([(si skipped?) (struct-info s)])
                                    (let loop ([si si])
                                      (let*-values ([(name init auto acc mut imms super skipped?) (struct-type-info si)])
                                        (let ([count (+ init auto)])
                                          (for ([i (in-range 0 count)])
                                            (mut s i (acc v i)))
                                          (when super
                                            (loop super))))))))
           s)])]
     [else
      (case v
        [(c)
         (let ([c (cons #f #f)])
	   (vector-set! fixup n (lambda (p)
                                  (error 'deserialize "cannot restore pair in cycle")))
	   c)]
	[(m) 
	 (let ([p0 (mcons #f #f)])
	   (vector-set! fixup n (lambda (p)
				  (set-mcar! p0 (mcar p))
				  (set-mcdr! p0 (mcdr p))))
	   p0)]
	[(b)
	 (let ([b0 (box #f)])
	   (vector-set! fixup n (lambda (b)
				  (set-box! b0 (unbox b))))
	   b0)]
	[(date)
         (error 'deserialize "cannot restore date in cycle")]
	[(arity-at-least)
         (error 'deserialize "cannot restore arity-at-least in cycle")]
	[(mpi)
         (error 'deserialize "cannot restore module-path-index in cycle")])]))

  (define (deserialize-with-map mod-map vers l module-path-index-join)
    (let ([share-n (list-ref l 2)]
          [shares (list-ref l 3)]
          [fixups (list-ref l 4)]
          [result (list-ref l 5)])
      ;; Create vector for sharing:
      (let* ([fixup (make-vector share-n #f)]
             [share (make-vector share-n (make-not-ready
                                          (list->vector shares)
                                          fixup))])
        ;; Deserialize into sharing array:
        (let loop ([n 0][l shares])
          (unless (= n share-n)
            (lookup-shared! share n mod-map module-path-index-join)
            (loop (add1 n) (cdr l))))
        ;; Fixup shell for graphs
        (for-each (lambda (n+v)
                    (let ([v (deserialize-one (cdr n+v) share mod-map module-path-index-join)])
                      ((vector-ref fixup (car n+v)) v)))
                  fixups)
        ;; Deserialize final result. (If there's no sharing, then
        ;;  all the work is actually here.)
        (deserialize-one result share mod-map module-path-index-join))))
  
  (define (extract-version l)
    (if (pair? (car l))
        (values (caar l) (cdr l))
        (values 0 l)))

  (define (deserialize l)
    (let-values ([(vers l) (extract-version l)])
      (let ([mod-map (make-vector (list-ref l 0))]
            [mod-map-l (list-ref l 1)])
        ;; Load constructor mapping
        (let loop ([n 0][l mod-map-l])
          (unless (null? l)
            (let* ([path+name (car l)]
                   [des (if (car path+name)
                            (let ([p (unprotect-path (car path+name))]
                                  [sym (revive-symbol (cdr path+name))])
                              ((deserialize-module-guard) p sym)
                              (dynamic-require p sym))
                            (namespace-variable-value (cdr path+name)))])
              ;; Register maker and struct type:
              (vector-set! mod-map n des))
            (loop (add1 n) (cdr l))))
        (deserialize-with-map mod-map vers l module-path-index-join))))

  ;; ----------------------------------------

  (define (serialized=? l1 l2)
    (let-values ([(vers1 l1) (extract-version l1)]
                 [(vers2 l2) (extract-version l2)])
      (let ([mod-map1 (make-vector (list-ref l1 0))]
            [mod-map1-l (list-ref l1 1)]
            [mod-map2 (make-vector (list-ref l2 0))]
            [mod-map2-l (list-ref l2 1)]
            [make-key (lambda (path+name)
                        (if (car path+name)
                            (let ([p (unprotect-path (car path+name))]
                                  [sym (revive-symbol (cdr path+name))])
                              (list p sym))
                            (list #f (cdr path+name))))]
            [mpi-key (gensym)])
        (let ([keys1 (map make-key mod-map1-l)]
              [keys2 (map make-key mod-map2-l)]
              [ht (make-hash)]
              [mpij (lambda (a b) (vector mpi-key a b))])
          (for-each (lambda (key)
                      (unless (hash-ref ht key #f)
                        (hash-set! ht key (gensym))))
                    (append keys1 keys2))
          (for-each (lambda (mod-map keys)
                      (let loop ([n 0][l keys])
                        (unless (null? l)
                          (let ([sym (hash-ref ht (car l))])
                            (vector-set! mod-map n
                                         (make-deserialize-info
                                          (lambda args
                                            (vector sym (list->vector args)))
                                          (lambda ()
                                            (let ([v (vector sym #f)])
                                              (values v
                                                      (lambda (vec)
                                                        (vector-set! v 1 (vector-ref vec 1)))))))))
                          (loop (add1 n) (cdr l)))))
                    (list mod-map1 mod-map2)
                    (list keys1 keys2))
          (let ([v1 (deserialize-with-map mod-map1 vers1 l1 mpij)]
                [v2 (deserialize-with-map mod-map2 vers2 l2 mpij)])
            (equal? v1 v2)))))))
