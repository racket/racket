#lang scheme/base
(require "../utils/utils.ss")

(require (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)
	 (types utils comparison resolve abbrev)
         (env type-name-env)
         (only-in (infer infer-dummy) unify)
         scheme/match
         mzlib/trace
	 (for-syntax scheme/base stxclass))

;; exn representing failure of subtyping
;; s,t both types

(define-struct (exn:subtype exn:fail) (s t))

;; inference failure - masked before it gets to the user program
(define-syntax fail!
  (syntax-rules ()
    [(_ s t) (raise (make-exn:subtype "subtyping failed" (current-continuation-marks) s t))]))

;; data structures for remembering things on recursive calls
(define (empty-set) '())

(define current-seen (make-parameter (empty-set)))

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


;; the algorithm for recursive types transcribed directly from TAPL, pg 305
;; List[(cons Number Number)] type type -> List[(cons Number Number)]
;; potentially raises exn:subtype, when the algorithm fails
;; is s a subtype of t, taking into account constraints A
(define (subtype* A s t)
  (parameterize ([match-equality-test type-equal?]
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
            (match (list s t)
	      [(list _ (Univ:)) A0]
	      ;; error is top and bot
	      [(list _ (Error:)) A0]
	      [(list (Error:) _) A0]
	      ;; (Un) is bot
	      [(list _ (Union: (list))) (fail! s t)]
	      [(list (Union: (list)) _) A0]
	      ;; value types              
	      [(list (Value: v1) (Value: v2)) (=> unmatch) (if (equal? v1 v2) A0 (unmatch))]
	      ;; integers are numbers too
	      [(list (Base: 'Integer _) (Base: 'Number _)) A0]
	      ;; values are subtypes of their "type"
	      [(list (Value: (? integer? n)) (Base: 'Integer _)) A0]
	      [(list (Value: (? number? n)) (Base: 'Number _)) A0]
	      [(list (Value: (? boolean? n)) (Base: 'Boolean _)) A0]
	      [(list (Value: (? symbol? n)) (Base: 'Symbol _)) A0]
	      [(list (Value: (? string? n)) (Base: 'String _)) A0]
	      ;; tvars are equal if they are the same variable
	      [(list (F: t) (F: t*)) (if (eq? t t*) A0 (fail! s t))]
	      ;; case-lambda
	      [(list (Function: arr1) (Function: arr2))
	       (when (null? arr1) (fail! s t))
	       (let loop-arities ([A* A0]
				  [arr2 arr2])
		 (cond 
		  [(null? arr2) A*]
		  [(supertype-of-one/arr A* (car arr2) arr1) => (lambda (A) (loop-arities A (cdr arr2)))]
		  [else (fail! s t)]))]
	      ;; recur structurally on pairs
	      [(list (Pair: a d) (Pair: a* d*))
	       (let ([A1 (subtype* A0 a a*)])
		 (and A1 (subtype* A1 d d*)))]
	      ;; quantification over two types preserves subtyping
	      [(list (Poly: ns b1) (Poly: ms b2)) 
	       (=> unmatch)
	       (unless (= (length ns) (length ms)) 
		       (unmatch))
					;(printf "Poly: ~n~a ~n~a~n" b1 (subst-all (map list ms (map make-F ns)) b2))
	       (subtype* A0 b1 (subst-all (map list ms (map make-F ns)) b2))]
	      [(list (Refinement: par _ _) t)
               (subtype* A0 par t)]
	      ;; use unification to see if we can use the polytype here
	      [(list (Poly: vs b) s)
	       (=> unmatch)
	       (if (unify vs (list b) (list s)) A0 (unmatch))]              
	      [(list s (Poly: vs b))
	       (=> unmatch)
	       (if (null? (fv b)) (subtype* A0 s b) (unmatch))]
	      ;; rec types, applications and names (that aren't the same
	      [(list (? needs-resolving? s) other)
               (let ([s* (resolve-once s)])
                 (if (Type? s*) ;; needed in case this was a name that hasn't been resolved yet
                     (subtype* A0 s* other)
                     (fail! s t)))]
	      [(list other (? needs-resolving? t))
               (let ([t* (resolve-once t)])
                 (if (Type? t*) ;; needed in case this was a name that hasn't been resolved yet
                     (subtype* A0 other t*)
                     (fail! s t)))]
	      ;; for unions, we check the cross-product
	      [(list (Union: es) t) (and (andmap (lambda (elem) (subtype* A0 elem t)) es) A0)]
	      [(list s (Union: es)) (and (ormap (lambda (elem) (subtype*/no-fail A0 s elem)) es) A0)]
	      ;; subtyping on immutable structs is covariant
	      [(list (Struct: nm _ flds #f _ _ _) (Struct: nm _ flds* #f _ _ _))
	       (subtypes* A0 flds flds*)]
	      [(list (Struct: nm _ flds proc _ _ _) (Struct: nm _ flds* proc* _ _ _))
	       (subtypes* A0 (cons proc flds) (cons proc* flds*))]
	      ;; subtyping on structs follows the declared hierarchy
	      [(list (Struct: nm (? Type? parent) flds proc _ _ _) other) 
					;(printf "subtype - hierarchy : ~a ~a ~a~n" nm parent other)
	       (subtype* A0 parent other)]
	      ;; Promises are covariant
	      [(list (Struct: 'Promise _ (list t) _ _ _ _) (Struct: 'Promise _ (list t*) _ _ _ _)) (subtype* A0 t t*)]
	      ;; subtyping on values is pointwise
	      [(list (Values: vals1) (Values: vals2)) (subtypes* A0 vals1 vals2)]
              ;; trivial case for Result
              [(list (Result: t f o) (Result: t* f o))
               (subtype* A0 t t*)]
              ;; we can ignore interesting results
              [(list (Result: t f o) (Result: t* (LFilterSet: (list) (list)) (LEmpty:)))
               (subtype* A0 t t*)]
	      ;; subtyping on other stuff
	      [(list (Syntax: t) (Syntax: t*))
	       (subtype* A0 t t*)]
	      [(list (Instance: t) (Instance: t*))
	       (subtype* A0 t t*)]
	      ;; otherwise, not a subtype
	      [_ (fail! s t) #;(printf "failed")])))]))))

  (define (type-compare? a b)
  (and (subtype a b) (subtype b a)))

(provide subtype type-compare? subtypes/varargs subtypes)

;(trace subtype*)
;(trace supertype-of-one/arr)
;(trace arr-subtype*/no-fail)
;(trace subtype-of-one)
;(trace subtype*/no-fail)
;(trace subtypes*)
;(trace subtype)

;(subtype (-> Univ B) (-> Univ Univ))
;(subtype (make-poly '(a) (make-tvar 'a)) (make-lst N))

;;problem:
;; (subtype (make-Mu 'x (make-Syntax (make-Union (list (make-Base 'Number #'number?) (make-F 'x))))) (make-Syntax (make-Univ)))
