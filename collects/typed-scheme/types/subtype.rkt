#lang scheme/base
(require "../utils/utils.rkt"
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)
	 (types utils comparison resolve abbrev substitute)
         (env type-name-env)
         (only-in (infer infer-dummy) unify)
         scheme/match unstable/match
         mzlib/trace (rename-in scheme/contract
                                [-> c->]
                                [->* c->*])
	 (for-syntax scheme/base syntax/parse))

;; exn representing failure of subtyping
;; s,t both types

(define-struct (exn:subtype exn:fail) (s t))

;; subtyping failure - masked before it gets to the user program
(define-syntax fail!
  (syntax-rules ()
    [(_ s t) (raise (make-exn:subtype "subtyping failed" (current-continuation-marks) s t))]))

;; data structures for remembering things on recursive calls
(define (empty-set) '())

(define current-seen (make-parameter (empty-set) #;pair?))

(define (seen-before s t) (cons (Type-seq s) (Type-seq t)))
(define (remember s t A) (cons (seen-before s t) A))
(define (seen? s t) (member (seen-before s t) (current-seen)))

(define subtype-cache (make-hash))
(define (cache-types s t)
  (cache-keys (Type-seq s) (Type-seq t)))
(define (cache-keys ks kt)
  (hash-set! subtype-cache (cons ks kt) #t))
(define (cached? s t)
  (hash-ref subtype-cache (cons (Type-seq s) (Type-seq t)) #f))

;; is s a subtype of t?
;; type type -> boolean
(define (subtype s t)
  (define k (cons (Type-seq s) (Type-seq t)))
  (define lookup? (hash-ref subtype-cache k 'no))
  (if (eq? 'no lookup?)
      (let ([result (with-handlers
                        ([exn:subtype? (lambda _ #f)])
                      (subtype* (current-seen) s t))])
        (hash-set! subtype-cache k result)
        result)
      lookup?))

;; are all the s's subtypes of all the t's?
;; [type] [type] -> boolean
(define (subtypes s t)
  (with-handlers
      ([exn:subtype? (lambda _ #f)])
    (subtypes* (current-seen) s t)))

;; subtyping under constraint set, but produces boolean result instead of raising exn
;; List[(cons Number Number)] type type -> maybe[List[(cons Number Number)]]
(define (subtype*/no-fail A s t)
  (with-handlers
      ([exn:subtype? (lambda _ #f)])
    (subtype* A s t)))

;; type type -> (does not return)
;; subtying fails
#;
(define (fail! s t) (raise (make-exn:subtype "subtyping failed" (current-continuation-marks) s t)))

;; check subtyping for two lists of types
;; List[(cons Number Number)] listof[type] listof[type] -> List[(cons Number Number)]
(define (subtypes* A ss ts)
  (cond [(and (null? ss) (null? ts) A)]
        [(or (null? ss) (null? ts)) (fail! ss ts)]
        [(subtype* A (car ss) (car ts)) 
         =>
         (lambda (A*) (subtypes* A* (cdr ss) (cdr ts)))]
        [else (fail! (car ss) (car ts))]))

;; check if s is a supertype of any element of ts
(define (supertype-of-one/arr A s ts)
  (ormap (lambda (e) (arr-subtype*/no-fail A e s)) ts))

(define-syntax (subtype-seq stx)
  (define-syntax-class sub*
    (pattern e:expr))
  (syntax-parse stx
    [(_ init (s1:sub* . args1) (s:sub* . args) ...)
     (with-syntax ([(A* ... A-last) (generate-temporaries #'(s1 s ...))])
       (with-syntax ([(clauses ...)
		      (for/list ([s (syntax->list #'(s1 s ...))]
				 [args (syntax->list #'(args1 args ...))]
				 [A (syntax->list #'(init A* ...))]
				 [A-next (syntax->list #'(A* ... A-last))])
			 #`[#,A-next (#,s #,A . #,args)])])
	#'(let* (clauses ...)
	    A-last)))]))

(define (kw-subtypes* A0 t-kws s-kws)
  (let loop ([A A0] [t t-kws] [s s-kws])    
    (match* (t s)
      [((list (Keyword: kt tt rt) rest-t) (list (Keyword: ks ts rs) rest-s))
       (cond [(eq? kt ks)
              (if  
               ;; if s is optional, t must be as well
               (or rs (not rt))
               (loop (subtype A tt ts) rest-t rest-s)
               (fail! t s))]
             ;; extra keywords in t are ok
             ;; we just ignore them
             [(keyword<? kt ks) (loop A rest-t s)]
             ;; extra keywords in s are a problem
             [else (fail! t s)])]
      ;; no more keywords to satisfy
      [(_ '()) A]
      ;; we failed to satisfy all the keyword
      [(_ _) (fail! s t)])))

;; simple co/contra-variance for ->
(define (arr-subtype*/no-fail A0 s t)
  (with-handlers
      ([exn:subtype? (lambda _ #f)])
    (match* (s t)
      ;; top for functions is above everything
      [(_ (top-arr:)) A0]
      ;; the really simple case
      [((arr: s1 s2 #f #f '())
        (arr: t1 t2 #f #f '()))
       (subtype-seq A0
                    (subtypes* t1 s1)
                    (subtype* s2 t2))]
      [((arr: s1 s2 #f #f s-kws)
        (arr: t1 t2 #f #f t-kws))
       (subtype-seq A0
                    (subtypes* t1 s1)
                    (kw-subtypes* t-kws s-kws)
                    (subtype* s2 t2))]
      [((arr: s-dom s-rng s-rest #f s-kws)
        (arr: t-dom t-rng #f #f t-kws))
       (subtype-seq A0
                    (subtypes*/varargs t-dom s-dom s-rest)
                    (kw-subtypes* t-kws s-kws)
                    (subtype* s-rng t-rng))]
      [((arr: s-dom s-rng #f #f s-kws)
        (arr: t-dom t-rng t-rest #f t-kws))
       (fail! s t)]
      [((arr: s-dom s-rng s-rest #f s-kws)
        (arr: t-dom t-rng t-rest #f t-kws))
       (subtype-seq A0
                    (subtypes*/varargs t-dom s-dom s-rest)
                    (subtype* t-rest s-rest)
                    (kw-subtypes* t-kws s-kws)
                    (subtype* s-rng t-rng))]
      ;; handle ... varargs when the bounds are the same
      [((arr: s-dom s-rng #f (cons s-drest dbound) s-kws)
        (arr: t-dom t-rng #f (cons t-drest dbound) t-kws))
       (subtype-seq A0
                    (subtype* t-drest s-drest)
                    (subtypes* t-dom s-dom)
                    (kw-subtypes* t-kws s-kws)
                    (subtype* s-rng t-rng))]
      [(_ _) 
       (fail! s t)])))

(define (subtypes/varargs args dom rst)
  (with-handlers
      ([exn:subtype? (lambda _ #f)])      
    (subtypes*/varargs (empty-set) args dom rst)))

(define (subtypes*/varargs A0 argtys dom rst)
  (let loop-varargs ([dom dom] [argtys argtys] [A A0])
    (cond
      [(and (null? dom) (null? argtys)) A]
      [(null? argtys) (fail! argtys dom)]
      [(and (null? dom) rst)
       (cond [(subtype* A (car argtys) rst) => (lambda (A) (loop-varargs dom (cdr argtys) A))]
             [else (fail! (car argtys) rst)])]
      [(null? dom) (fail! argtys dom)]
      [(subtype* A (car argtys) (car dom)) => (lambda (A) (loop-varargs (cdr dom) (cdr argtys) A))]
      [else (fail! (car argtys) (car dom))])))

;(trace subtypes*/varargs)

(d/c (combine-arrs arrs)
  (c-> (listof arr?) (or/c #f arr?))
  (match arrs
    [(list (and a1 (arr: dom1 rng1 #f #f '())) (arr: dom rng #f #f '()) ...)
     (cond
       [(null? dom) (make-arr dom1 rng1 #f #f '())]
       [(not (apply = (length dom1) (map length dom))) #f]
       [(not (for/and ([rng2 (in-list rng)]) (type-equal? rng1 rng2)))
        #f]
       [else (make-arr (apply map (lambda args (make-Union (sort args type<?))) (cons dom1 dom)) rng1 #f #f '())])]
    [_ #f]))

(define (subtype/flds* A flds flds*)
  (for/fold ([A A]) ([f (in-list flds)] [f* (in-list flds*)])
    (match* (f f*)
      [((fld: t _ #t) (fld: t* _ #t))
       (subtype* (subtype* A t* t) t t*)]
      [((fld: t _ #f) (fld: t* _ #f))
       (subtype* A t t*)])))

;; the algorithm for recursive types transcribed directly from TAPL, pg 305
;; List[(cons Number Number)] type type -> List[(cons Number Number)]
;; potentially raises exn:subtype, when the algorithm fails
;; is s a subtype of t, taking into account constraints A
(define (subtype* A s t)
  (define =t (lambda (a b) (if (and (Rep? a) (Rep? b)) (type-equal? a b) (equal? a b))))
  (parameterize ([match-equality-test =t]
                 [current-seen A])
    (let ([ks (Type-key s)] [kt (Type-key t)])
      (cond 
       [(or (seen? s t) (type-equal? s t)) A]
       [(and (symbol? ks) (symbol? kt) (not (eq? ks kt))) (fail! s t)]
       [(and (symbol? ks) (pair? kt) (not (memq ks kt))) (fail! s t)]
       [(and (pair? ks) (pair? kt)
	     (for/and ([i (in-list ks)]) (not (memq i kt))))
	(fail! s t)]
       [else
	(let* ([A0 (remember s t A)])
	  (parameterize ([current-seen A0])
            (match* (s t)
	      [(_ (Univ:)) A0]
	      ;; error is top and bot
	      [(_ (Error:)) A0]
	      [((Error:) _) A0]
	      ;; (Un) is bot
	      [(_ (Union: (list))) (fail! s t)]
	      [((Union: (list)) _) A0]
	      ;; value types              
	      [((Value: v1) (Value: v2)) (=> unmatch) (if (equal? v1 v2) A0 (unmatch))]
	      ;; now we encode the numeric hierarchy - bletch
	      [((Base: 'Integer _) (Base: 'Number _)) A0]
	      [((Base: 'Flonum _)  (== -Real =t)) A0]
	      [((Base: 'Integer _)  (== -Real =t)) A0]
              [((Base: 'Flonum _)  (Base: 'InexactComplex _)) A0]
	      [((Base: 'Flonum _)  (Base: 'Number _)) A0]
	      [((Base: 'Exact-Rational _) (Base: 'Number _)) A0]
	      [((Base: 'Integer _) (Base: 'Exact-Rational _)) A0]
	      [((Base: 'Exact-Positive-Integer _) (Base: 'Exact-Rational _)) A0]
	      [((Base: 'Exact-Positive-Integer _) (Base: 'Number _)) A0]
	      [((Base: 'Exact-Positive-Integer _) (== -Nat =t)) A0]
	      [((Base: 'Exact-Positive-Integer _) (Base: 'Integer _)) A0]

              [((Base: 'Positive-Fixnum _) (Base: 'Exact-Positive-Integer _)) A0]
	      [((Base: 'Positive-Fixnum _) (Base: 'Exact-Rational _)) A0]
	      [((Base: 'Positive-Fixnum _) (Base: 'Number _)) A0]
	      [((Base: 'Positive-Fixnum _) (== -Nat =t)) A0]
	      [((Base: 'Positive-Fixnum _) (Base: 'Integer _)) A0]

	      [((Base: 'Negative-Fixnum _) (Base: 'Exact-Rational _)) A0]
	      [((Base: 'Negative-Fixnum _) (Base: 'Number _)) A0]
	      [((Base: 'Negative-Fixnum _) (Base: 'Integer _)) A0]
              
	      [((== -Nat =t) (Base: 'Number _)) A0]
	      [((== -Nat =t) (Base: 'Exact-Rational _)) A0]
	      [((== -Nat =t) (Base: 'Integer _)) A0]

	      [((== -Fixnum =t) (Base: 'Number _)) A0]
	      [((== -Fixnum =t) (Base: 'Exact-Rational _)) A0]
	      [((== -Fixnum =t) (Base: 'Integer _)) A0]

              [((Base: 'Nonnegative-Flonum _) (Base: 'Flonum _)) A0]
              [((Base: 'Nonnegative-Flonum _) (Base: 'InexactComplex _)) A0]
              [((Base: 'Nonnegative-Flonum _) (Base: 'Number _)) A0]

              [((Base: 'InexactComplex _) (Base: 'Number _)) A0]

              
              ;; values are subtypes of their "type"
	      [((Value: (? exact-integer? n)) (Base: 'Integer _)) A0]
	      [((Value: (and n (? number?) (? exact?) (? rational?))) (Base: 'Exact-Rational _)) A0]
	      [((Value: (? exact-nonnegative-integer? n)) (== -Nat =t)) A0]
	      [((Value: (? exact-positive-integer? n)) (Base: 'Exact-Positive-Integer _)) A0]	      
	      [((Value: (? inexact-real? n)) (Base: 'Flonum _)) A0]
	      [((Value: (? real? n)) (== -Real =t)) A0]
	      [((Value: (? number? n)) (Base: 'Number _)) A0]

              [((Value: (? keyword?)) (Base: 'Keyword _)) A0]
              [((Value: (? char?)) (Base: 'Char _)) A0]
	      [((Value: (? boolean? n)) (Base: 'Boolean _)) A0]
	      [((Value: (? symbol? n)) (Base: 'Symbol _)) A0]
	      [((Value: (? string? n)) (Base: 'String _)) A0]
	      ;; tvars are equal if they are the same variable
	      [((F: t) (F: t*)) (if (eq? t t*) A0 (fail! s t))]              
              ;; sequences are covariant
              [((Sequence: ts) (Sequence: ts*))
               (subtypes* A0 ts ts*)]
              [((Listof: t) (Sequence: (list t*)))
               (subtype* A0 t t*)]
              [((List: ts) (Sequence: (list t*)))
               (subtypes* A0 ts (map (λ _ t*) ts))]
              [((HeterogenousVector: ts) (Sequence: (list t*)))
               (subtypes* A0 ts (map (λ _ t*) ts))]
              [((Vector: t) (Sequence: (list t*)))
               (subtype* A0 t t*)]
              [((Base: 'String _) (Sequence: (list t*)))
               (subtype* A0 -Char t*)]
              [((Base: 'Bytes _) (Sequence: (list t*)))
               (subtype* A0 -Nat t*)]
              [((Base: 'Input-Port _) (Sequence: (list t*)))
               (subtype* A0 -Nat t*)]
              [((Hashtable: k v) (Sequence: (list k* v*)))
               (subtypes* A0 (list k v) (list k* v*))]
              ;; special-case for case-lambda/union
              [((Function: arr1) (Function: (list arr2)))
               (when (null? arr1) (fail! s t))
               (or (arr-subtype*/no-fail A0 (combine-arrs arr1) arr2)
                   (supertype-of-one/arr A0 arr2 arr1)
                   (fail! s t))]
	      ;; case-lambda
	      [((Function: arr1) (Function: arr2))
	       (when (null? arr1) (fail! s t))
	       (let loop-arities ([A* A0]
				  [arr2 arr2])
		 (cond 
		  [(null? arr2) A*]
		  [(supertype-of-one/arr A* (car arr2) arr1) => (lambda (A) (loop-arities A (cdr arr2)))]
		  [else (fail! s t)]))]
	      ;; recur structurally on pairs
	      [((Pair: a d) (Pair: a* d*))
	       (let ([A1 (subtype* A0 a a*)])
		 (and A1 (subtype* A1 d d*)))]
              ;; recur structurally on dotted lists, assuming same bounds
              [((ListDots: s-dty dbound) (ListDots: t-dty dbound))
               (subtype* A0 s-dty t-dty)]
              [((ListDots: s-dty dbound) (Listof: t-elem))
               (subtype* A0 (substitute Univ dbound s-dty) t-elem)]
	      ;; quantification over two types preserves subtyping
	      [((Poly: ns b1) (Poly: ms b2)) 
	       (=> unmatch)
	       (unless (= (length ns) (length ms)) 
		       (unmatch))
	       (subtype* A0 b1 (subst-all (make-simple-substitution ms (map make-F ns)) b2))]
	      [((Refinement: par _ _) t)
               (subtype* A0 par t)]
	      ;; use unification to see if we can use the polytype here
	      [((Poly: vs b) s)
	       (=> unmatch)
	       (if (unify vs (list b) (list s)) A0 (unmatch))]              
	      [(s (Poly: vs b))
	       (=> unmatch)
	       (if (null? (fv b)) (subtype* A0 s b) (unmatch))]
	      ;; rec types, applications and names (that aren't the same
	      [((? needs-resolving? s) other)
               (let ([s* (resolve-once s)])
                 (if (Type? s*) ;; needed in case this was a name that hasn't been resolved yet
                     (subtype* A0 s* other)
                     (fail! s t)))]
	      [(other (? needs-resolving? t))
               (let ([t* (resolve-once t)])
                 (if (Type? t*) ;; needed in case this was a name that hasn't been resolved yet
                     (subtype* A0 other t*)
                     (fail! s t)))]
	      ;; for unions, we check the cross-product
	      [((Union: es) t) (or (and (andmap (lambda (elem) (subtype* A0 elem t)) es) A0)
                                   (fail! s t))]
	      [(s (Union: es)) (or (and (ormap (lambda (elem) (subtype*/no-fail A0 s elem)) es) A0)
                                   (fail! s t))]
	      ;; subtyping on immutable structs is covariant	      
	      [((Struct: nm _ flds proc _ _ _ _) (Struct: nm _ flds* proc* _ _ _ _))
               (let ([A (cond [(and proc proc*) (subtype* proc proc*)]
                              [proc* (fail! proc proc*)]
                              [else A0])])
                 (subtype/flds* A flds flds*))]
              [((Struct: _ _ _ _ _ _ _ _) (StructTop: (== s type-equal?)))
               A0]
              [((Box: _) (BoxTop:)) A0]
              [((Channel: _) (ChannelTop:)) A0]
              [((Vector: _) (VectorTop:)) A0]
              [((HeterogenousVector: _) (VectorTop:)) A0]
              [((HeterogenousVector: (list e ...)) (Vector: e*))
               (if (andmap (lambda (e0) (type-equal? e0 e*)) e) A0 (fail! s t))]
              [((MPair: _ _) (MPairTop:)) A0]
              [((Hashtable: _ _) (HashtableTop:)) A0]
	      ;; subtyping on structs follows the declared hierarchy
	      [((Struct: nm (? Type? parent) flds proc _ _ _ _) other) 
               ;(printf "subtype - hierarchy : ~a ~a ~a~n" nm parent other)
	       (subtype* A0 parent other)]
	      ;; Promises are covariant
	      [((Struct: (== promise-sym) _ (list t) _ _ _ _ _) (Struct: (== promise-sym) _ (list t*) _ _ _ _ _)) (subtype* A0 t t*)]
	      ;; subtyping on values is pointwise
	      [((Values: vals1) (Values: vals2)) (subtypes* A0 vals1 vals2)]
              ;; trivial case for Result
              [((Result: t f o) (Result: t* f o))
               (subtype* A0 t t*)]
              ;; we can ignore interesting results
              [((Result: t f o) (Result: t* (FilterSet: (Top:) (Top:)) (Empty:)))
               (subtype* A0 t t*)]
	      ;; subtyping on other stuff
	      [((Syntax: t) (Syntax: t*))
	       (subtype* A0 t t*)]
	      [((Instance: t) (Instance: t*))
	       (subtype* A0 t t*)]
              [((Class: '() '() (list (and s  (list names  meths )) ...))
                (Class: '() '() (list (and s* (list names* meths*)) ...)))
               (for/fold ([A A0]) 
                         ([n names*] [m meths*])
                         (cond [(assq n s) => (lambda (spec) (subtype* A (cadr spec) m))]
                               [else (fail! s t)]))]
	      ;; otherwise, not a subtype
	      [(_ _) (fail! s t) #;(printf "failed")])))]))))

  (define (type-compare? a b)
  (and (subtype a b) (subtype b a)))

(provide subtype type-compare? subtypes/varargs subtypes)

;(trace subtype*)
;(trace supertype-of-one/arr)
;(trace arr-subtype*/no-fail)
;(trace subtype*/no-fail)
;(trace subtypes*)
;(trace subtype)

;(subtype (-> Univ B) (-> Univ Univ))
;(subtype (make-poly '(a) (make-tvar 'a)) (make-lst N))

;;problem:
;; (subtype (make-Mu 'x (make-Syntax (make-Union (list (make-Base 'Number #'number?) (make-F 'x))))) (make-Syntax (make-Univ)))
