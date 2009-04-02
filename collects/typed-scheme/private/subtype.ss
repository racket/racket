#lang scheme/base
(require "../utils/utils.ss")

(require (except-in (rep type-rep effect-rep rep-utils) sub-eff)
         (utils tc-utils)
	 "type-utils.ss"
         "type-comparison.ss"
         "resolve-type.ss"
         "type-abbrev.ss"
         (env type-name-env)
         (only-in (infer infer-dummy) unify)
         scheme/match
         mzlib/trace)



;; exn representing failure of subtyping
;; s,t both types

(define-struct (exn:subtype exn:fail) (s t))
#;
(define-values (fail-sym exn:subtype?)
  (let ([sym (gensym)])
    (values sym (lambda (s) (eq? s sym)))))

;; inference failure - masked before it gets to the user program
(define-syntax fail!
  (syntax-rules ()
    [(_ s t) #;(raise fail-sym)
             (raise (make-exn:subtype "subtyping failed" (current-continuation-marks) s t))
             #;(error "inference failed" s t)]))


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

(define (sub-eff e1 e2)
  (match* (e1 e2)
    [(e e) #t]
    [((Latent-Restrict-Effect: t) (Latent-Restrict-Effect: t*))
     (and (subtype t t*)
          (subtype t* t))]
    [((Latent-Remove-Effect: t) (Latent-Remove-Effect: t*))
     (and (subtype t t*)
          (subtype t* t))]
    [(_ _) #f]))

;(trace sub-eff)


;; simple co/contra-variance for ->
(define (arr-subtype*/no-fail A0 s t)
  (with-handlers
      ([exn:subtype? (lambda _ #f)])
    (match (list s t)
      ;; top for functions is above everything
      [(list _ (top-arr:)) A0]
      [(list (arr: s1 s2 #f #f (list (cons kw s-kw-ty) ...) thn-eff els-eff) 
             (arr: t1 t2 #f #f (list (cons kw t-kw-ty) ...) thn-eff  els-eff))       
       (let* ([A1 (subtypes* A0 t1 s1)]
              [A2 (subtypes* A1 t-kw-ty s-kw-ty)])
         (subtype* A1 s2 t2))]
      [(list (arr: s1 s2 s3 #f (list (cons kw s-kw-ty) ...) thn-eff els-eff)
             (arr: t1 t2 t3 #f (list (cons kw t-kw-ty) ...) thn-eff* els-eff*))
       (unless 
           (or (and (null? thn-eff*) (null? els-eff*))
               (and (effects-equal? thn-eff thn-eff*)
                    (effects-equal? els-eff els-eff*))
               (and 
                (= (length thn-eff) (length thn-eff*))
                (= (length els-eff) (length els-eff*))
                (andmap sub-eff thn-eff thn-eff*)
                (andmap sub-eff els-eff els-eff*)))
         (fail! s t))
       ;; either the effects have to be the same, or the supertype can't have effects
       (let* ([A2 (subtypes*/varargs A0 t1 s1 s3)]
              [A3 (subtypes* A2 t-kw-ty s-kw-ty)])
         (if (not t3)
             (subtype* A3 s2 t2)
             (let ([A1 (subtype* A3 t3 s3)])
               (subtype* A1 s2 t2))))]
      [else 
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
              ;; A refinement is a subtype of its parent
              [(list (Refinement: par _ _) t)
               (subtype* A0 par t)]
	      ;; use unification to see if we can use the polytype here
	      [(list (Poly: vs b) s)
	       (=> unmatch)
	       (if (unify vs (list b) (list s)) A0 (unmatch))]              
	      [(list s (Poly: vs b))
	       (=> unmatch)
	       (if (null? (fv b)) (subtype* A0 s b) (unmatch))]
	      ;; names are compared for equality:
	      [(list (Name: n) (Name: n*))
	       (=> unmatch)
	       (if (free-identifier=? n n*)
		   A0
		   (unmatch))]
	      ;; just unfold the recursive types
	      [(list _ (? Mu?)) (subtype* A0 s (unfold t))]
	      [(list (? Mu?) _) (subtype* A0 (unfold s) t)]
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
	      ;; applications and names are structs too
	      [(list (App: (Name: n) args stx) other)
	       (let ([t (lookup-type-name n)])
		 (unless (Type? t)                     
			 (fail! s t))
		 #;(printf "subtype: app-name: name: ~a type: ~a other: ~a ~ninst: ~a~n" (syntax-e n) t other 
		 (instantiate-poly t args))
		 (unless (Poly? t)
			 (tc-error/stx stx "cannot apply non-polymorphic type ~a" t))                 
		 (match t [(Poly-unsafe: n _)
			   (unless (= n (length args))
				   (tc-error/stx stx "wrong number of arguments to polymorphic type: expected ~a and got ~a"
						 n (length args)))])
		 (let ([v (subtype* A0 (instantiate-poly t args) other)])
		   #;(printf "val: ~a~n"  v)
		   v))]
	      [(list other (App: (Name: n) args stx))
	       (let ([t (lookup-type-name n)])
		 (unless (Type? t)                     
			 (fail! s t))
		 #;(printf "subtype: 2 app-name: name: ~a type: ~a other: ~a ~ninst: ~a~n" (syntax-e n) t other 
		 (instantiate-poly t args))
		 (unless (Poly? t)
			 (tc-error/stx stx "cannot apply non-polymorphic type ~a" t))                 
		 (match t [(Poly-unsafe: n _)
			   (unless (= n (length args))
				   (tc-error/stx stx "wrong number of arguments to polymorphic type: expected ~a and got ~a"
						 n (length args)))])
					;(printf "about to call subtype with: ~a ~a ~n" other (instantiate-poly t args))
		 (let ([v (subtype* A0 other (instantiate-poly t args))])
		   #;(printf "2 val: ~a~n"  v)
		   v))]
	      [(list (Name: n) other)
	       (let ([t (lookup-type-name n)])
					;(printf "subtype: name: ~a ~a ~a~n" (syntax-e n) t other)
		 (if (Type? t)                     
		     (subtype* A0 t other)
		     (fail! s t)))]
	      ;; Promises are covariant
	      [(list (Struct: 'Promise _ (list t) _ _ _ _) (Struct: 'Promise _ (list t*) _ _ _ _)) (subtype* A0 t t*)]
	      ;; subtyping on values is pointwise
	      [(list (Values: vals1) (Values: vals2)) (subtypes* A0 vals1 vals2)]
	      ;; single values shouldn't actually happen, but they're just like the type
	      [(list t (Values: (list t*))) (int-err "BUG - singleton values type~a" (make-Values (list t*)))]
	      [(list (Values: (list t)) t*) (int-err "BUG - singleton values type~a" (make-Values (list t)))]              
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