#lang racket/unit

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         "signatures.rkt" "tc-metafunctions.rkt" "check-below.rkt"
         "tc-app-helper.rkt" "find-annotation.rkt" "tc-funapp.rkt"
         "tc-subst.rkt" (prefix-in c: racket/contract)
         syntax/parse racket/match racket/trace scheme/list
         unstable/sequence  unstable/list
         ;; fixme - don't need to be bound in this phase - only to make tests work
         scheme/bool
         racket/unsafe/ops
         (only-in racket/private/class-internal make-object do-make-object)
         (only-in '#%kernel [apply k:apply] [reverse k:reverse])
         ;; end fixme
         (for-syntax syntax/parse scheme/base (utils tc-utils))
         (private type-annotation)
         (types utils abbrev union subtype resolve convenience type-table substitute)
         (utils tc-utils)
         (only-in srfi/1 alist-delete)
         (except-in (env type-env-structs tvar-env index-env) extend)
         (rep type-rep filter-rep object-rep rep-utils)
         (r:infer infer)
         '#%paramz
         (for-template
          racket/unsafe/ops racket/fixnum racket/flonum
          (only-in '#%kernel [apply k:apply] [reverse k:reverse])
          "internal-forms.rkt" scheme/base scheme/bool '#%paramz
          (only-in racket/private/class-internal make-object do-make-object)))

(import tc-expr^ tc-lambda^ tc-let^ tc-apply^)
(export tc-app^)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comparators

;; comparators that inform the type system
(define-syntax-class comparator
  #:literals (eq? equal? eqv? = string=? symbol=? memq member memv)
  (pattern eq?) (pattern equal?) (pattern eqv?) (pattern =) (pattern string=?) (pattern symbol=?)
  (pattern member) (pattern memq) (pattern memv))

;; typecheck eq? applications
;; identifier expr expr -> tc-results
(define (tc/eq comparator v1 v2)
  (define (eq?-able e) (or (boolean? e) (keyword? e) (symbol? e) (eof-object? e)))
  (define (eqv?-able e) (or (eq?-able e) (number? e)))
  (define (equal?-able e) #t)
  (define (ok? val)
    (define-syntax-rule (alt nm pred ...) (and (free-identifier=? #'nm comparator) (or (pred val) ...)))
    (or (alt symbol=? symbol?)
        (alt string=? string?)
        (alt = number?)
        (alt eq? eq?-able)
        (alt eqv? eqv?-able)
        (alt equal? equal?-able)))
  (match* ((single-value v1) (single-value v2))
    [((tc-result1: t _ o) (tc-result1: (Value: (? ok? val))))
     (ret -Boolean
	  (-FS (-filter-at (-val val) o)
	       (-not-filter-at (-val val) o)))]
    [((tc-result1: (Value: (? ok? val))) (tc-result1: t _ o))
     (ret -Boolean
	  (-FS (-filter-at (-val val) o)
	       (-not-filter-at (-val val) o)))]
    [((tc-result1: t _ o)
      (or (and (? (lambda _ (free-identifier=? #'member comparator)))
	       (tc-result1: (app untuple (list (and ts (Value: _)) ...))))
	  (and (? (lambda _ (free-identifier=? #'memv comparator)))
	       (tc-result1: (app untuple (list (and ts (Value: (? eqv?-able))) ...))))
	  (and (? (lambda _ (free-identifier=? #'memq comparator)))
	       (tc-result1: (app untuple (list (and ts (Value: (? eq?-able))) ...))))))
     (let ([ty (apply Un ts)])
       (ret (Un (-val #f) t)
	    (-FS (-filter-at ty o)
		 (-not-filter-at ty o))))]
    [(_ _) (ret -Boolean)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keywords

(define (tc-keywords/internal arity kws kw-args error?)
  (match arity
    [(arr: dom rng rest #f ktys)
     ;; assumes that everything is in sorted order
     (let loop ([actual-kws kws]
                [actuals (map tc-expr/t (syntax->list kw-args))]
                [formals ktys])
       (match* (actual-kws formals)
         [('() '())
          (void)]
         [(_ '())
          (if error?
              (tc-error/expr #:return (ret (Un))
                             "Unexpected keyword argument ~a" (car actual-kws))
              #f)]
         [('() (cons fst rst))
          (match fst
            [(Keyword: k _ #t)
             (if error?
                 (tc-error/expr #:return (ret (Un))
                                "Missing keyword argument ~a" k)
                 #f)]
            [_ (loop actual-kws actuals rst)])]
         [((cons k kws-rest) (cons (Keyword: k* t req?) form-rest))
          (cond [(eq? k k*) ;; we have a match
                 (if (subtype (car actuals) t)
                     ;; success
                     (loop kws-rest (cdr actuals) form-rest)
                     ;; failure
                     (and error?
                          (tc-error/delayed
                           "Wrong function argument type, expected ~a, got ~a for keyword argument ~a"
                           t (car actuals) k)
                          (loop kws-rest (cdr actuals) form-rest)))]
                [req? ;; this keyword argument was required
                 (if error?
                     (begin (tc-error/delayed "Missing keyword argument ~a" k*)
                            (loop kws-rest (cdr actuals) form-rest))
                     #f)]
                [else ;; otherwise, ignore this formal param, and continue
                 (loop actual-kws actuals form-rest)])]))]))

(define (tc-keywords form arities kws kw-args pos-args expected)
  (match arities
    [(list (and a (arr: dom rng rest #f ktys)))
     (tc-keywords/internal a kws kw-args #t)
     (tc/funapp (car (syntax-e form)) kw-args
                (ret (make-Function (list (make-arr* dom rng #:rest rest))))
                (map tc-expr (syntax->list pos-args)) expected)]
    [(list (and a (arr: doms rngs rests (and drests #f) ktyss)) ...)
     (let ([new-arities
            (for/list ([a (in-list arities)]
                       ;; find all the arities where the keywords match
                       #:when (tc-keywords/internal a kws kw-args #f))
              (match a
                [(arr: dom rng rest #f ktys) (make-arr* dom rng #:rest rest)]))])
       (if (null? new-arities)
           (domain-mismatches
            (car (syntax-e form)) (cdr (syntax-e form))
            arities doms rests drests rngs
            (map tc-expr (syntax->list pos-args))
            #f #f #:expected expected
            #:return (or expected (ret (Un)))
            #:msg-thunk
            (lambda (dom)
              (string-append "No function domains matched in function application:\n"
                             dom)))
           (tc/funapp (car (syntax-e form)) kw-args
                      (ret (make-Function new-arities))
                      (map tc-expr (syntax->list pos-args)) expected)))]))

(define (type->list t)
  (match t
    [(Pair: (Value: (? keyword? k)) b) (cons k (type->list b))]
    [(Value: '()) null]
    [_ (int-err "bad value in type->list: ~a" t)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objects

(define (check-do-make-object cl pos-args names named-args)
  (let* ([names (map syntax-e (syntax->list names))]
         [name-assoc (map list names (syntax->list named-args))])
    (let loop ([t (tc-expr cl)])
      (match t
        [(tc-result1: (? Mu? t*)) (loop (ret (unfold t*)))]
        [(tc-result1: (Union: '())) (ret (Un))]
        [(tc-result1: (and c (Class: pos-tys (list (and tnflds (list tnames _ _)) ...) _)))
         (unless (= (length pos-tys)
                    (length (syntax->list pos-args)))
           (tc-error/delayed "expected ~a positional arguments, but got ~a"
                             (length pos-tys) (length (syntax->list pos-args))))
         ;; use for, since they might be different lengths in error case
         (for ([pa (in-syntax pos-args)]
               [pt (in-list pos-tys)])
           (tc-expr/check pa (ret pt)))
         (for ([n names]
               #:unless (memq n tnames))
           (tc-error/delayed
            "unknown named argument ~a for class\nlegal named arguments are ~a"
            n (stringify tnames)))
         (for-each (match-lambda
                     [(list tname tfty opt?)
                      (let ([s (cond [(assq tname name-assoc) => cadr]
                                     [(not opt?)
                                      (tc-error/delayed "value not provided for named init arg ~a" tname)
                                      #f]
                                     [else #f])])
                        (if s
                            ;; this argument was present
                            (tc-expr/check s (ret tfty))
                            ;; this argument wasn't provided, and was optional
                            #f))])
                   tnflds)
         (ret (make-Instance c))]
        [(tc-result1: t)
         (tc-error/expr #:return (ret (Un)) "expected a class value for object creation, got: ~a" t)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; let loop

(define (let-loop-check form lam lp actuals args body expected)
  (syntax-parse #`(#,args #,body #,actuals)
    #:literals (#%plain-app if null? pair? null)
    [((val acc ...)
      ((~and inner-body (if (#%plain-app (~or pair? null?) val*) thn els)))
      (actual actuals ...))
     #:when
     (and (free-identifier=? #'val #'val*)
          (ormap (lambda (a) (find-annotation #'inner-body a))
                 (syntax->list #'(acc ...))))
     (let* ([ts1 (generalize (tc-expr/t #'actual))]
            [ann-ts (for/list ([a (in-syntax #'(acc ...))]
                               [ac (in-syntax #'(actuals ...))])
                      (or (find-annotation #'inner-body a)
                          (generalize (tc-expr/t ac))))]
            [ts (cons ts1 ann-ts)])
       ;; check that the actual arguments are ok here
       (for/list ([a (syntax->list #'(actuals ...))]
                  [t ann-ts])
         (tc-expr/check a (ret t)))
       ;; then check that the function typechecks with the inferred types
       (add-typeof-expr lam (tc/rec-lambda/check form args body lp ts expected))
       expected)]
    ;; special case `for/list'
    [((val acc ...)
      ((~and inner-body (if e1 e2 e3:id)))
      (null actuals ...))
     #:when (free-identifier=? #'val #'e3)
     (let ([ts (for/list ([ac (syntax->list #'(actuals ...))]
                          [f (syntax->list #'(acc ...))])
                 (or
                  (type-annotation f #:infer #t)
                  (generalize (tc-expr/t ac))))]
           [acc-ty (or
                    (type-annotation #'val #:infer #t)
                    (match expected
                      [(tc-result1: (and t (Listof: _))) t]
                      [_ #f])
                    (generalize (-val '())))])
       (add-typeof-expr lam (tc/rec-lambda/check form args body lp (cons acc-ty ts) expected))
       expected)]
    ;; special case when argument needs inference
    [(_ body* _)
     (let ([ts (for/list ([ac (syntax->list actuals)]
                          [f (syntax->list args)])
                 (let* ([infer-t (or (type-annotation f #:infer #t)
                                     (find-annotation #'(begin . body*) f))])
                   (if infer-t
                       (begin (check-below (tc-expr/t ac) infer-t)
                              infer-t)
                       (generalize (tc-expr/t ac)))))])
       (add-typeof-expr lam (tc/rec-lambda/check form args body lp ts expected))
       expected)]))


;; the main dispatching function
;; syntax tc-results? -> tc-results?
(define (tc/app/internal form expected)
  (syntax-parse form
    #:literals (#%plain-app #%plain-lambda letrec-values quote
                values apply k:apply not false? list list* call-with-values do-make-object make-object cons
                map andmap ormap reverse k:reverse extend-parameterization
                vector-ref unsafe-vector-ref unsafe-vector*-ref
                vector-set! unsafe-vector-set! unsafe-vector*-set!
                unsafe-struct-ref unsafe-struct*-ref unsafe-struct-set! unsafe-struct*-set!)
    [(#%plain-app extend-parameterization pmz args ...)
     (let loop ([args (syntax->list #'(args ...))])
       (if (null? args) (ret Univ)
           (let* ([p (car args)]
                  [pt (single-value p)]
                  [v (cadr args)]
                  [vt (single-value v)])
             (match pt
               [(tc-result1: (Param: a b))
                (check-below vt a)
                (loop (cddr args))]
               [(tc-result1: t)
                (tc-error/expr #:return (or expected (ret Univ)) "expected Parameter, but got ~a" t)
                (loop (cddr args))]))))]
    ;; use the additional but normally ignored first argument to make-sequence to provide a better instantiation
    [(#%plain-app (~var op (id-from 'make-sequence 'racket/private/for)) (~and quo ((~literal quote) (i:id ...))) arg:expr)
     #:when (andmap type-annotation (syntax->list #'(i ...)))
     (match (single-value #'op)
         [(tc-result1: (and t Poly?))
          (tc-expr/check #'quo (ret Univ))
          (tc/funapp #'op #'(quo arg)
                     (ret (instantiate-poly t (extend (list Univ Univ)
                                                      (map type-annotation (syntax->list #'(i ...)))
                                                      Univ)))
                     (list (ret Univ) (single-value #'arg))
                     expected)])]
    ;; unsafe struct operations
    [(#%plain-app (~and op (~or (~literal unsafe-struct-ref) (~literal unsafe-struct*-ref))) s e:expr)
     (let ([e-t (single-value #'e)])
       (match (single-value #'s)
         [(tc-result1:
           (and t (or (Struct: _ _ (list (fld: flds _ muts) ...) _ _ _ _ _)
                      (? needs-resolving?
                         (app resolve-once
                              (Struct: _ _ (list (fld: flds _ muts) ...) _ _ _ _ _))))))
          (let ([ival (or (syntax-parse #'e [((~literal quote) i:number) (syntax-e #'i)] [_ #f])
                          (match e-t
                            [(tc-result1: (Value: (? number? i))) i]
                            [_ #f]))])
            (cond [(not ival)
                   (check-below e-t -Integer)
                   (if expected
                       (check-below (ret (apply Un flds)) expected)
                       (ret (apply Un flds)))]
                  [(and (integer? ival) (exact? ival) (<= 0 ival (sub1 (length flds))))
                   (let ([result (if (list-ref muts ival)
                                     (ret (list-ref flds ival))
                                     ;; FIXME - could do something with paths here
                                     (ret (list-ref flds ival)))])
                     (if expected (check-below result expected) result))]
                  [(not (and (integer? ival) (exact? ival)))
                   (tc-error/expr #:stx #'e #:return (or expected (ret (Un))) "expected exact integer for struct index, but got ~a" ival)]
                  [(< ival 0)
                   (tc-error/expr #:stx #'e #:return (or expected (ret (Un))) "index ~a too small for struct ~a" ival t)]
                  [(not (<= ival (sub1 (length flds))))
                   (tc-error/expr #:stx #'e #:return (or expected (ret (Un))) "index ~a too large for struct ~a" ival t)]))]
         [s-ty
          (let ([arg-tys (list s-ty e-t)])
            (tc/funapp #'op #'(s e) (single-value #'op) arg-tys expected))]))]
    [(#%plain-app (~and op (~or (~literal unsafe-struct-set!) (~literal unsafe-struct*-set!))) s e:expr val:expr)
     (let ([e-t (single-value #'e)])
       (match (single-value #'s)
         [(tc-result1: (and t (or (Struct: _ _ (list (fld: flds _ _) ...) _ _ _ _ _)
                                  (? needs-resolving?
                                     (app resolve-once
                                          (Struct: _ _ (list (fld: flds _ _) ...) _ _ _ _ _))))))
          (let ([ival (or (syntax-parse #'e [((~literal quote) i:number) (syntax-e #'i)] [_ #f])
                          (match e-t
                            [(tc-result1: (Value: (? number? i))) i]
                            [_ #f]))])
            (cond [(not ival)
                   (tc-error/expr #:stx #'e
                                  #:return (or expected (ret -Void))
                                  "expected statically known index for unsafe struct mutation, but got ~a" (match e-t [(tc-result1: t) t]))]
                  [(and (integer? ival) (exact? ival) (<= 0 ival (sub1 (length flds))))
                   (tc-expr/check #'val (ret (list-ref flds ival)))
                   (if expected
                       (check-below (ret -Void) expected)
                       (ret -Void))]
                  [(not (and (integer? ival) (exact? ival)))
                   (single-value #'val)
                   (tc-error/expr #:stx #'e #:return (or expected (ret (Un))) "expected exact integer for unsafe struct mutation, but got ~a" ival)]
                  [(< ival 0)
                   (single-value #'val)
                   (tc-error/expr #:stx #'e #:return (or expected (ret (Un))) "index ~a too small for struct ~a" ival t)]
                  [(not (<= ival (sub1 (length flds))))
                   (single-value #'val)
                   (tc-error/expr #:stx #'e #:return (or expected (ret (Un))) "index ~a too large for struct ~a" ival t)]))]
         [s-ty
          (let ([arg-tys (list s-ty e-t (single-value #'val))])
            (tc/funapp #'op #'(s e val) (single-value #'op) arg-tys expected))]))]
    ;; vector-ref on het vectors
    [(#%plain-app (~and op (~or (~literal vector-ref) (~literal unsafe-vector-ref) (~literal unsafe-vector*-ref))) v e:expr)
     (let ([e-t (single-value #'e)])
       (let loop ((v-t (single-value #'v)))
	 (match v-t
           [(tc-result1: (and t (HeterogenousVector: es)))
	    (let ([ival (or (syntax-parse #'e [((~literal quote) i:number) (syntax-e #'i)] [_ #f])
			    (match e-t
			      [(tc-result1: (Value: (? number? i))) i]
			      [_ #f]))])
	      (cond [(not ival)
		     (check-below e-t -Integer)
		     (if expected
			 (check-below (ret (apply Un es)) expected)
			 (ret (apply Un es)))]
		    [(and (integer? ival) (exact? ival) (<= 0 ival (sub1 (length es))))
		     (if expected
			 (check-below (ret (list-ref es ival)) expected)
			 (ret (list-ref es ival)))]
		    [(not (and (integer? ival) (exact? ival)))
		     (tc-error/expr #:stx #'e #:return (or expected (ret (Un))) "expected exact integer for vector index, but got ~a" ival)]
		    [(< ival 0)
		     (tc-error/expr #:stx #'e #:return (or expected (ret (Un))) "index ~a too small for vector ~a" ival t)]
		    [(not (<= ival (sub1 (length es))))
		     (tc-error/expr #:stx #'e #:return (or expected (ret (Un))) "index ~a too large for vector ~a" ival t)]))]
	   [(tc-result1: (? needs-resolving? e) f o)
	    (loop (ret (resolve-once e) f o))]
	   [v-ty
	    (let ([arg-tys (list v-ty e-t)])
	      (tc/funapp #'op #'(v e) (single-value #'op) arg-tys expected))])))]
    [(#%plain-app (~and op (~or (~literal vector-set!) (~literal unsafe-vector-set!) (~literal unsafe-vector*-set!))) v e:expr val:expr)
     (let ([e-t (single-value #'e)])
       (let loop ((v-t (single-value #'v)))
	 (match v-t
           [(tc-result1: (and t (HeterogenousVector: es)))
	    (let ([ival (or (syntax-parse #'e [((~literal quote) i:number) (syntax-e #'i)] [_ #f])
			    (match e-t
			      [(tc-result1: (Value: (? number? i))) i]
			      [_ #f]))])
	      (cond [(not ival)
		     (tc-error/expr #:stx #'e
				    #:return (or expected (ret -Void))
				    "expected statically known index for heterogeneous vector, but got ~a" (match e-t [(tc-result1: t) t]))]
		    [(and (integer? ival) (exact? ival) (<= 0 ival (sub1 (length es))))
		     (tc-expr/check #'val (ret (list-ref es ival)))
		     (if expected
			 (check-below (ret -Void) expected)
			 (ret -Void))]
		    [(not (and (integer? ival) (exact? ival)))
		     (single-value #'val)
		     (tc-error/expr #:stx #'e #:return (or expected (ret (Un))) "expected exact integer for vector index, but got ~a" ival)]
		    [(< ival 0)
		     (single-value #'val)
		     (tc-error/expr #:stx #'e #:return (or expected (ret (Un))) "index ~a too small for vector ~a" ival t)]
		    [(not (<= ival (sub1 (length es))))
		     (single-value #'val)
		     (tc-error/expr #:stx #'e #:return (or expected (ret (Un))) "index ~a too large for vector ~a" ival t)]))]
	   [(tc-result1: (? needs-resolving? e) f o)
	    (loop (ret (resolve-once e) f o))]
	   [v-ty
	    (let ([arg-tys (list v-ty e-t (single-value #'val))])
	      (tc/funapp #'op #'(v e val) (single-value #'op) arg-tys expected))])))]
    [(#%plain-app (~and op (~or (~literal vector-immutable) (~literal vector))) args:expr ...)
     (let loop ([expected expected])
       (match expected
         [(tc-result1: (Vector: t))
          (for ([e (in-list (syntax->list #'(args ...)))])
            (tc-expr/check e (ret t)))
          expected]
         [(tc-result1: (HeterogenousVector: ts))
          (unless (= (length ts) (length (syntax->list #'(args ...))))
            (tc-error/expr "expected vector with ~a elements, but got ~a"
                           (length ts)
                           (make-HeterogenousVector (map tc-expr/t (syntax->list #'(args ...))))))
          (for ([e (in-list (syntax->list #'(args ...)))]
                [t (in-list ts)])
            (tc-expr/check e (ret t)))
          expected]
         [(tc-result1: (? needs-resolving? e) f o)
          (loop (ret (resolve-once e) f o))]
         [(tc-result1: (and T (Union: (app (λ (ts)
                                             (for/list ([t ts]
                                                        #:when (let ([k (Type-key t)])
                                                                 (eq? 'vector k)))
                                               t))
                                         ts))))
          (if (null? ts)
            (let ([arg-tys (map single-value (syntax->list #'(args ...)))])
              (tc/funapp #'op #'(args ...) (single-value #'op) arg-tys expected))
            (check-below (for/first ([t ts]) (loop (ret t)))
                         expected))]
	 ;; since vectors are mutable, if there is no expected type, we want to generalize the element type
         [(or #f (tc-result1: _))
	  (ret (make-HeterogenousVector (map (lambda (x) (generalize (tc-expr/t x)))
					     (syntax->list #'(args ...)))))]
         [_ (int-err "bad expected: ~a" expected)]))]
    ;; since vectors are mutable, if there is no expected type, we want to generalize the element type
    [(#%plain-app (~and op (~literal make-vector)) n elt)
     (match expected
       [(tc-result1: (Vector: t))
        (tc-expr/check #'n (ret -Integer))
        (tc-expr/check #'elt (ret t))
        expected]
       [(or #f (tc-result1: _))
        (tc/funapp #'op #'(n elt) (single-value #'op)
                   (list (single-value #'n)
                         (match (single-value #'elt)
                           [(tc-result1: t) (ret (generalize t))]))
                   expected)]
       [_ (int-err "bad expected: ~a" expected)])]
    [(#%plain-app (~and op (~literal build-vector)) n proc)
     (match expected
       [(tc-result1: (Vector: t))
        (tc-expr/check #'n (ret -Integer))
        (tc-expr/check #'proc (ret (-NonNegFixnum . -> . t)))
        expected]
       [(or #f (tc-result1: _))
        (tc/funapp #'op #'(n elt) (single-value #'op)
                   (list (single-value #'n)
                         (match (tc/funapp #'proc #'(1) ; valid nonnegative-fixnum
                                           (single-value #'proc)
                                           (list (ret -NonNegFixnum))
                                           #f)
                           [(tc-result1: t) (ret (-> -NonNegFixnum (generalize t)))]))
                   expected)]
       [_ (int-err "bad expected: ~a" expected)])]
    ;; special case for `-' used like `sub1'
    [(#%plain-app (~and op (~literal -)) v (~and arg2 ((~literal quote) 1)))
     (add-typeof-expr #'arg2 (ret -PosFixnum))
     (match-let ([(tc-result1: t) (single-value #'v)])
       (cond
        [(subtype t -PosFixnum) (ret -NonNegFixnum)]
        [(subtype t -NonNegFixnum) (ret -Fixnum)]
        [(subtype t -PosInt) (ret -Nat)]
        [else (tc/funapp #'op #'(v arg2) (single-value #'op) (list (ret t) (single-value #'arg2)) expected)]))]
    ;; idem for fx-
    [(#%plain-app (~and op (~or (~literal fx-) (~literal unsafe-fx-))) v (~and arg2 ((~literal quote) 1)))
     (add-typeof-expr #'arg2 (ret -PosFixnum))
     (match-let ([(tc-result1: t) (single-value #'v)])
       (cond
        [(subtype t -PosInt) (ret -NonNegFixnum)]
        [else (tc/funapp #'op #'(v arg2) (single-value #'op) (list (ret t) (single-value #'arg2)) expected)]))]
    ;; call-with-values
    [(#%plain-app call-with-values prod con)
     (match (tc/funapp #'prod #'() (single-value #'prod) null #f)
       [(tc-results: ts fs os)
        (tc/funapp #'con #'(prod) (single-value #'con) (map ret ts fs os) expected)])]
    ;; in eq? cases, call tc/eq
    [(#%plain-app eq?:comparator v1 v2)
     ;; make sure the whole expression is type correct
     (match* ((tc/funapp #'eq? #'(v1 v2) (single-value #'eq?) (map single-value (syntax->list #'(v1 v2))) expected)
              ;; check thn and els with the eq? info
              (tc/eq #'eq? #'v1 #'v2))
       [((tc-result1: t) (tc-result1: t* f o))
        (ret t f o)])]
    ;; special-case for not - flip the filters
    [(#%plain-app (~or false? not) arg)
     (match (single-value #'arg)
       [(tc-result1: t (FilterSet: f+ f-) _)
        (ret -Boolean (make-FilterSet f- f+))])]
    ;; (apply values l) gets special handling
    [(#%plain-app apply values e)
     (match (single-value #'e)
       [(tc-result1: (ListDots: dty dbound)) (values->tc-results (make-ValuesDots null dty dbound) #f)]
       [(tc-result1: (List: ts)) (ret ts)]
       [_ (tc/apply #'values #'(e))])]
    ;; rewrite this so that it takes advantages of all the special cases
    [(#%plain-app k:apply . args) (tc/app/internal (syntax/loc form (#%plain-app apply . args)) expected)]
    ;; handle apply specially
    [(#%plain-app apply f . args) (tc/apply #'f #'args)]
    ;; special case for `values' with single argument - we just ignore the values, except that it forces arg to return one value
    [(#%plain-app values arg)
     (match expected
      [#f (single-value #'arg)]
      [(tc-result1: tp)
       (single-value #'arg expected)]
      [(tc-results: ts)
       (single-value #'arg) ;Type check the argument, to find other errors
       (tc-error/expr #:return expected
         "wrong number of values: expected ~a but got one"
          (length ts))])]
    ;; handle `values' specially
    [(#%plain-app values . args)
     (match expected
       [(tc-results: ets efs eos)
        (match-let ([(list (tc-result1: ts fs os) ...)
                     (for/list ([arg (syntax->list #'args)]
                                [et ets] [ef efs] [eo eos])
                       (single-value arg (ret et ef eo)))])
          (if (= (length ts) (length ets) (length (syntax->list #'args)))
              (ret ts fs os)
              (tc-error/expr #:return expected "wrong number of values: expected ~a but got ~a"
                             (length ets) (length (syntax->list #'args)))))]
       [_ (match-let ([(list (tc-result1: ts fs os) ...)
                       (for/list ([arg (syntax->list #'args)])
                         (single-value arg))])
            (ret ts fs os))])]
    ;; special case for keywords
    [(#%plain-app
      (#%plain-app cpce s-kp fn kpe kws num)
      kw-list
      (#%plain-app list . kw-arg-list)
      . pos-args)
     #:declare cpce (id-from 'checked-procedure-check-and-extract 'racket/private/kw)
     #:declare s-kp (id-from 'struct:keyword-procedure 'racket/private/kw)
     #:declare kpe  (id-from 'keyword-procedure-extract 'racket/private/kw)
     (match (tc-expr #'fn)
       [(tc-result1: (Poly: vars
                            (Function: (list (and ar (arr: dom rng (and rest #f) (and drest #f) kw-formals))))))
        (=> fail)
        (unless (null? (fv/list kw-formals))
          (fail))
        (match (map single-value (syntax->list #'pos-args))
          [(list (tc-result1: argtys-t) ...)
           (let* ([subst (infer vars null argtys-t dom rng (and expected (tc-results->values expected)))])
             (unless subst (fail))
             (tc-keywords form (list (subst-all subst ar))
                          (type->list (tc-expr/t #'kws)) #'kw-arg-list #'pos-args expected))])]
       [(tc-result1: (Function: arities))
        (tc-keywords form arities (type->list (tc-expr/t #'kws)) #'kw-arg-list #'pos-args expected)]
       [(tc-result1: (Poly: _ (Function: _)))
        (tc-error/expr #:return (ret (Un))
                       "Inference for polymorphic keyword functions not supported")]
       [(tc-result1: t) (tc-error/expr #:return (ret (Un))
                                       "Cannot apply expression of type ~a, since it is not a function type" t)])]
    ;; even more special case for match
    [(#%plain-app (letrec-values ([(lp) (~and lam (#%plain-lambda args . body))]) lp*) . actuals)
     #:fail-unless expected #f
     #:fail-unless (not (andmap type-annotation (syntax->list #'(lp . args)))) #f
     #:fail-unless (free-identifier=? #'lp #'lp*) #f
     (let-loop-check form #'lam #'lp #'actuals #'args #'body expected)]
    ;; special cases for classes
    [(#%plain-app make-object cl . args)
     (check-do-make-object #'cl #'args #'() #'())]
    [(#%plain-app do-make-object cl (#%plain-app list . pos-args) (#%plain-app list (#%plain-app cons 'names named-args) ...))
     (check-do-make-object #'cl #'pos-args #'(names ...) #'(named-args ...))]
    [(#%plain-app (~and map-expr (~literal map)) f arg0 arg ...)
     (match* ((single-value #'arg0) (map single-value (syntax->list #'(arg ...))))
       ;; if the argument is a ListDots
       [((tc-result1: (ListDots: t0 bound0))
         (list (tc-result1: (or (and (ListDots: t bound) (app (λ _ #f) var))
                                ;; a devious hack - just generate #f so the test below succeeds
                                ;; have to explicitly bind `var' since otherwise `var' appears on only one side of the or
                                ;; NOTE: safe to include these, `map' will error if any list is not the same length as all the others
                                (and (Listof: t var) (app (λ _ #f) bound))))
               ...))
        (=> fail)
        (unless (for/and ([b bound]) (or (not b) (eq? bound0 b))) (fail))
        (match (extend-tvars (list bound0)
                 ;; just check that the function applies successfully to the element type
                 (tc/funapp #'f #'(arg0 arg ...) (tc-expr #'f) (cons (ret t0) (map ret t)) expected))
          [(tc-result1: t) (ret (make-ListDots t bound0))]
          [(tc-results: ts)
           (tc-error/expr #:return (ret (Un))
                          "Expected one value, but got ~a" (-values ts))])]
       ;; otherwise, if it's not a ListDots, defer to the regular function typechecking
       [(res0 res)
        (tc/funapp #'map-expr #'(f arg0 arg ...) (single-value #'map-expr) (list* (tc-expr #'f) res0 res) expected)])]
    ;; ormap/andmap of ... argument
    [(#%plain-app (~and fun (~or (~literal andmap) (~literal ormap))) f arg)
     ;; check the arguments
     (match-let* ([arg-ty (single-value #'arg)]
                  [ft (tc-expr #'f)])
       (match (match arg-ty
                ;; if the argument is a ListDots
                [(tc-result1: (ListDots: t bound))
                 ;; just check that the function applies successfully to the element type
                 (tc/funapp #'f #'(arg) ft (list (ret (substitute Univ bound t))) expected)]
                ;; otherwise ...
                [_ #f])
         [(tc-result1: t) (ret (Un (-val #f) t))]
         ;; if it's not a ListDots, defer to the regular function typechecking
         [_ (tc/funapp #'fun #'(f arg) (single-value #'fun) (list ft arg-ty) expected)]))]
    ;; special case for `delay'
    [(#%plain-app
      mp1
      (#%plain-lambda ()
        (#%plain-app mp2 (#%plain-app call-with-values (#%plain-lambda () e) list))))
     #:declare mp1 (id-from 'make-promise 'scheme/promise)
     #:declare mp2 (id-from 'make-promise 'scheme/promise)
     (ret (-Promise (tc-expr/t #'e)))]
    ;; special case for `list'
    [(#%plain-app list . args)
     (begin
       ;(printf "calling list: ~a ~a\n" (syntax->datum #'args) expected)
       (match expected
         [(tc-result1: (Mu: var (Union: (or
                                         (list (Pair: elem-ty (F: var)) (Value: '()))
                                         (list (Value: '()) (Pair: elem-ty (F: var)))))))
          ;(printf "special case 1 ~a\n" elem-ty)
          (for ([i (in-list (syntax->list #'args))])
               (tc-expr/check i (ret elem-ty)))
          expected]
         [(tc-result1: (app untuple (? (lambda (ts) (and ts (= (length (syntax->list #'args))
                                                               (length ts))))
                                       ts)))
          ;(printf "special case 2 ~a\n" ts)
          (for ([ac (in-list (syntax->list #'args))]
                [exp (in-list ts)])
               (tc-expr/check ac (ret exp)))
          expected]
         [_
          ;(printf "not special case\n")
          (let ([tys (map tc-expr/t (syntax->list #'args))])
            (ret (apply -lst* tys)))]))]
    ;; special case for `list*'
    [(#%plain-app list* . args)
     (match-let* ([(list last tys-r ...) (reverse (map tc-expr/t (syntax->list #'args)))]
                  [tys (reverse tys-r)])
       (ret (foldr make-Pair last tys)))]
    ;; special case for `reverse' to propagate expected type info
    [(#%plain-app (~or reverse k:reverse) arg)
     (match expected
       [(tc-result1: (Listof: _))
        (tc-expr/check #'arg expected)]
       [(tc-result1: (List: ts))
        (tc-expr/check #'arg (ret (-Tuple (reverse ts))))
        expected]
       [_
        (match (single-value #'arg)
          [(tc-result1: (List: ts))
           (if expected
               (check-below (ret (-Tuple (reverse ts))) expected)
               (ret (-Tuple (reverse ts))))]
          [arg-ty
           (tc/funapp #'reverse #'(arg) (single-value #'reverse) (list arg-ty) expected)])])]
    ;; inference for ((lambda
    [(#%plain-app (#%plain-lambda (x ...) . body) args ...)
     #:fail-unless (= (length (syntax->list #'(x ...)))
                      (length (syntax->list #'(args ...))))
     #f
     #:fail-when (andmap type-annotation (syntax->list #'(x ...))) #f
     (tc/let-values #'((x) ...) #'(args ...) #'body
                    #'(let-values ([(x) args] ...) . body)
                    expected)]
    ;; inference for ((lambda with dotted rest
    [(#%plain-app (#%plain-lambda (x ... . rst:id) . body) args ...)
     #:fail-unless (<= (length (syntax->list #'(x ...)))
                       (length (syntax->list #'(args ...)))) #f
    ;; FIXME - remove this restriction - doesn't work because the annotation
    ;; on rst is not a normal annotation, may have * or ...
     #:fail-when (type-annotation #'rst) #f
     #:fail-when (andmap type-annotation (syntax->list #'(x ...))) #f
     (let-values ([(fixed-args varargs) (split-at (syntax->list #'(args ...)) (length (syntax->list #'(x ...))))])
       (with-syntax ([(fixed-args ...) fixed-args]
                     [varg #`(#%plain-app list #,@varargs)])
         (tc/let-values #'((x) ... (rst)) #`(fixed-args ... varg) #'body
                        #'(let-values ([(x) fixed-args] ... [(rst) varg]) . body)
                        expected)))]
    [(#%plain-app f . args)
     (let* ([f-ty (single-value #'f)])
       (match f-ty
         [(tc-result1:
           (and t (Function:
                   (list (and a (arr: (? (lambda (d)
                                           (= (length d)
                                              (length (syntax->list #'args))))
                                         dom)
                                      (Values: (list (Result: v (FilterSet: (Top:) (Top:)) (Empty:))))
                                      #f #f (list (Keyword: _ _ #f) ...)))))))
          ;(printf "f dom: ~a ~a\n" (syntax->datum #'f) dom)
          (let ([arg-tys (map (lambda (a t) (tc-expr/check a (ret t)))
                              (syntax->list #'args)
                              dom)])
            (tc/funapp #'f #'args f-ty arg-tys expected))]
         [_
          (let ([arg-tys (map single-value (syntax->list #'args))])
            (tc/funapp #'f #'args f-ty arg-tys expected))]))]))

;(trace tc/app/internal)

;; syntax -> tc-results
(define (tc/app form) (tc/app/internal form #f))

;; syntax tc-results? -> tc-results?
(define (tc/app/check form expected)
    (define t (tc/app/internal form expected))
    (check-below t expected))

(define (object-index os i)
  (unless (number? i)
    (int-err "object-index for keywords NYI"))
  (list-ref os i))

;; in-indexes : Listof[Type] -> Sequence[index/c]
(define (in-indexes dom)
  (in-range (length dom)))



