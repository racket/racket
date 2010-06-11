#lang scheme/unit

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         "signatures.rkt" "tc-metafunctions.rkt"
         "tc-app-helper.rkt" "find-annotation.rkt"
         "tc-subst.rkt" (prefix-in c: scheme/contract)
         syntax/parse scheme/match mzlib/trace scheme/list 
	 unstable/sequence unstable/debug
         ;; fixme - don't need to be bound in this phase - only to make syntax/parse happy
         scheme/bool
         (only-in racket/private/class-internal make-object do-make-object)
         (only-in '#%kernel [apply k:apply])
         ;; end fixme
         (for-syntax syntax/parse scheme/base (utils tc-utils))
         (private type-annotation)
         (types utils abbrev union subtype resolve convenience type-table)
         (utils tc-utils)
         (only-in srfi/1 alist-delete)
         (except-in (env type-env-structs tvar-env index-env) extend)
         (rep type-rep filter-rep object-rep)
         (r:infer infer)
         '#%paramz
         (for-template 
          (only-in '#%kernel [apply k:apply])
          "internal-forms.rkt" scheme/base scheme/bool '#%paramz
          (only-in racket/private/class-internal make-object do-make-object)))

(import tc-expr^ tc-lambda^ tc-let^)
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
           (tc-error/expr 
            #:return (or expected (ret (Un)))
            (string-append "No function domains matched in function application:\n"
                           (domain-mismatches arities doms rests drests rngs (map tc-expr (syntax->list pos-args)) #f #f)))
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
               #:when (not (memq n tnames)))
           (tc-error/delayed 
            "unknown named argument ~a for class~nlegal named arguments are ~a"
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

(define (let-loop-check form lp actuals args body expected)
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
       (tc/rec-lambda/check form args body lp ts expected)
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
       (tc/rec-lambda/check form args body lp (cons acc-ty ts) expected)
       expected)]
    ;; special case when argument needs inference
    [(_ (body* ...) _)
     (let ([ts (for/list ([ac (syntax->list actuals)]
                          [f (syntax->list args)])
                 (let* ([infer-t (or (type-annotation f #:infer #t)
                                     (find-annotation #'(begin body* ...) f))])
                   (if infer-t
                       (begin (check-below (tc-expr/t ac) infer-t)
                              infer-t)
                       (generalize (tc-expr/t ac)))))])
       (tc/rec-lambda/check form args body lp ts expected)
       expected)]))

(define (tc/apply f args)
  (define (do-ret t)
    (match t 
      [(Values: (list (Result: ts _ _) ...)) (ret ts)]
      [(ValuesDots: (list (Result: ts _ _) ...) dty dbound) (ret ts (for/list ([t ts]) (-FS null null)) (for/list ([t ts]) (make-Empty)) dty dbound)]
      [_ (int-err "do-ret fails: ~a" t)]))
  (define f-ty (single-value f))
  ;; produces the first n-1 elements of the list, and the last element
  (define (split l) (let-values ([(f r) (split-at l (sub1 (length l)))])
                      (values f (car r))))
  (define-values (fixed-args tail)
    (let ([args* (syntax->list args)])
      (if (null? args*)
          (tc-error "apply requires a final list argument, given only a function argument of type ~a" (match f-ty [(tc-result1: t) t]))
          (split args*))))

  (match f-ty
    ;; apply of simple function
    [(tc-result1: (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ...)))
     ;; special case for (case-lambda)
     (when (null? doms)
       (tc-error/expr #:return (ret (Un))
                      "empty case-lambda given as argument to apply"))
     (match-let ([arg-tys (map tc-expr/t fixed-args)]
                 [(tc-result1: tail-ty) (single-value tail)])
       (let loop ([doms* doms] [rngs* rngs] [rests* rests] [drests* drests])
         (cond 
           ;; we've run out of cases to try, so error out           
           [(null? doms*)
            (tc-error/expr #:return (ret (Un))
                           (string-append 
                            "Bad arguments to function in apply:~n"
                            (domain-mismatches f-ty doms rests drests rngs arg-tys tail-ty #f)))]
           ;; this case of the function type has a rest argument
           [(and (car rests*)
                 ;; check that the tail expression is a subtype of the rest argument
                 (subtype (apply -lst* arg-tys #:tail tail-ty)
                          (apply -lst* (car doms*) #:tail (make-Listof (car rests*)))))
            (do-ret (car rngs*))]
           ;; the function expects a dotted rest arg, so make sure we have a ListDots
           [(and (car drests*)
                 (match tail-ty
                   [(ListDots: tail-ty tail-bound)
                    ;; the check that it's the same bound
                    (and (eq? (cdr (car drests*)) tail-bound)
                         ;; and that the types are correct
                         (subtypes arg-tys (car doms*))
                         (subtype tail-ty (car (car drests*))))]
                   [_ #f]))
            (do-ret (car rngs*))]
           ;; otherwise, nothing worked, move on to the next case
           [else (loop (cdr doms*) (cdr rngs*) (cdr rests*) (cdr drests*))])))]
    ;; apply of simple polymorphic function
    [(tc-result1: (Poly: vars (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ..1))))
     (let*-values ([(arg-tys) (map tc-expr/t fixed-args)]
                   [(tail-ty tail-bound) (match (tc-expr/t tail)
                                           [(ListDots: tail-ty tail-bound)
                                            (values tail-ty tail-bound)]
                                           [t (values t #f)])])
       (let loop ([doms* doms] [rngs* rngs] [rests* rests] [drests* drests])
         (cond [(null? doms*)
                (match f-ty 
                  [(tc-result1: (Poly-names: _ (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ..1))))
                   (tc-error/expr #:return (ret (Un))
                                 (string-append 
                                  "Bad arguments to polymorphic function in apply:~n"
                                  (domain-mismatches f-ty doms rests drests rngs arg-tys tail-ty tail-bound)))])]
               ;; the actual work, when we have a * function and a list final argument
               [(and (car rests*)
                     (not tail-bound)
                     (<= (length (car doms*))
                         (length arg-tys))
                     (infer/vararg vars null
                                   (cons tail-ty arg-tys) 
                                   (cons (make-Listof (car rests*))
                                         (car doms*))
                                   (car rests*)
                                   (car rngs*)))
                => (lambda (substitution) (do-ret (subst-all substitution (car rngs*))))]
               ;; actual work, when we have a * function and ... final arg
               [(and (car rests*)
                     tail-bound                     
                     (<= (length (car doms*))
                         (length arg-tys))
                     (infer/vararg vars null
                                   (cons (make-Listof tail-ty) arg-tys)
                                   (cons (make-Listof (car rests*))
                                         (car doms*))
                                   (car rests*)
                                   (car rngs*)))
                => (lambda (substitution) (do-ret (subst-all substitution (car rngs*))))]
               ;; ... function, ... arg
               [(and (car drests*)                     
                     tail-bound
                     (eq? tail-bound (cdr (car drests*)))
                     (= (length (car doms*))
                        (length arg-tys))
                     (infer vars null (cons tail-ty arg-tys) (cons (car (car drests*)) (car doms*)) 
                            (car rngs*)))
                => (lambda (substitution) (do-ret (subst-all substitution (car rngs*))))]
               ;; if nothing matches, around the loop again
               [else (loop (cdr doms*) (cdr rngs*) (cdr rests*) (cdr drests*))])))]
    [(tc-result1: (Poly: vars (Function: '())))
     (tc-error/expr #:return (ret (Un))
                    "Function has no cases")]
    [(tc-result1: (PolyDots: (and vars (list fixed-vars ... dotted-var))
                            (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ..1))))
     (let*-values ([(arg-tys) (map tc-expr/t fixed-args)]
                   [(tail-ty tail-bound) (match (tc-expr/t tail)
                                           [(ListDots: tail-ty tail-bound)
                                            (values tail-ty tail-bound)]
                                           [t (values t #f)])])
       (let loop ([doms* doms] [rngs* rngs] [rests* rests] [drests* drests])
         (cond [(null? doms*)
                (match f-ty 
                  [(tc-result1: (PolyDots-names: _ (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ..1))))
                   (tc-error/expr #:return (ret (Un))
                                 (string-append 
                                  "Bad arguments to polymorphic function in apply:~n"
                                  (domain-mismatches f-ty doms rests drests rngs arg-tys tail-ty tail-bound)))])]
               ;; the actual work, when we have a * function and a list final argument
               [(and (car rests*)
                     (not tail-bound)
                     (<= (length (car doms*))
                         (length arg-tys))
                     (infer/vararg fixed-vars (list dotted-var)
                                   (cons tail-ty arg-tys) 
                                   (cons (make-Listof (car rests*))
                                         (car doms*))
                                   (car rests*)
                                   (car rngs*)))
                => (lambda (substitution) (do-ret (subst-all substitution (car rngs*))))]
               ;; actual work, when we have a * function and ... final arg
               [(and (car rests*)
                     tail-bound                     
                     (<= (length (car doms*))
                         (length arg-tys))
                     (infer/vararg fixed-vars (list dotted-var)
                                   (cons (make-Listof tail-ty) arg-tys)
                                   (cons (make-Listof (car rests*))
                                         (car doms*))
                                   (car rests*)
                                   (car rngs*)))
                => (lambda (substitution) 
                     (do-ret (subst-all substitution (car rngs*))))]
               ;; ... function, ... arg, same bound on ...
               [(and (car drests*)
                     tail-bound
                     (eq? tail-bound (cdr (car drests*)))
                     (= (length (car doms*))
                        (length arg-tys))                     
                     (infer fixed-vars (list dotted-var)
                            (cons (make-ListDots tail-ty tail-bound) arg-tys)
                            (cons (make-ListDots (car (car drests*)) (cdr (car drests*))) (car doms*))
                            (car rngs*)))
                => (lambda (substitution)
                     (do-ret (subst-all substitution (car rngs*))))]
               ;; ... function, ... arg, different bound on ...
               [(and (car drests*)
                     tail-bound
                     (not (eq? tail-bound (cdr (car drests*))))
                     (= (length (car doms*))
                        (length arg-tys))
                     (extend-tvars (list tail-bound (cdr (car drests*)))
                       (extend-indexes (cdr (car drests*)) 
                         ;; don't need to add tail-bound - it must already be an index
                         (infer fixed-vars (list dotted-var)
                                (cons (make-ListDots tail-ty tail-bound) arg-tys)
                                (cons (make-ListDots (car (car drests*)) (cdr (car drests*))) (car doms*))
                                (car rngs*)))))
                => (lambda (substitution) 
                     (define drest-bound (cdr (car drests*)))
                     (let ([dots-subst (assq drest-bound substitution)])
                       (unless dots-subst
                         (int-err "expected dotted substitution for ~a" drest-bound))
                       (do-ret (substitute-dotted (cadr dots-subst)
                                                  tail-bound
                                                  drest-bound
                                                  (subst-all (alist-delete drest-bound substitution eq?)
                                                             (car rngs*))))))]
               ;; ... function, (List A B C etc) arg
               [(and (car drests*)
                     (not tail-bound)
                     (eq? (cdr (car drests*)) dotted-var)
                     (= (length (car doms*))
                        (length arg-tys))
                     (untuple tail-ty)
                     (infer/dots fixed-vars dotted-var (append arg-tys (untuple tail-ty)) (car doms*)
                                 (car (car drests*)) (car rngs*) (fv (car rngs*))))
                => (lambda (substitution) 
                     (define drest-bound (cdr (car drests*)))
                     (do-ret (subst-all substitution (car rngs*))))]
               ;; if nothing matches, around the loop again
               [else (loop (cdr doms*) (cdr rngs*) (cdr rests*) (cdr drests*))])))]
    [(tc-result1: (PolyDots: vars (Function: '())))
     (tc-error/expr #:return (ret (Un))
                    "Function has no cases")]
    [(tc-result1: f-ty) (tc-error/expr #:return (ret (Un))
                         "Type of argument to apply is not a function type: ~n~a" f-ty)]))

;; the main dispatching function
;; syntax tc-results? -> tc-results?
(define (tc/app/internal form expected)
  (syntax-parse form
    #:literals (#%plain-app #%plain-lambda letrec-values quote
                values apply k:apply not list list* call-with-values do-make-object make-object cons
                map andmap ormap reverse extend-parameterization vector-ref)
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
    ;; vector-ref on het vectors
    [(#%plain-app (~and op (~literal vector-ref)) v e:expr)
     (let ([e-t (single-value #'e)])
       (match (single-value #'v)
         [(tc-result1: (and t (HeterogenousVector: es)))
          (let ([ival (or (syntax-parse #'e [((~literal quote) i:number) (syntax-e #'i)] [_ #f])
                          (match e-t
                            [(tc-result1: (Value: (? number? i))) i]
                            [_ #f]))])
            (cond [(not ival)
                   (check-below e-t -Nat)
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
         [v-ty
          (let ([arg-tys (list v-ty e-t)])
            (tc/funapp #'op #'(v e) (single-value #'op) arg-tys expected))]))]
    [(#%plain-app (~and op (~literal vector-set!)) v e:expr val:expr)
     (let ([e-t (single-value #'e)])
       (match (single-value #'v)
         [(tc-result1: (and t (HeterogenousVector: es)))
          (let ([ival (or (syntax-parse #'e [((~literal quote) i:number) (syntax-e #'i)] [_ #f])
                          (match e-t
                            [(tc-result1: (Value: (? number? i))) i]
                            [_ #f]))])
            (cond [(not ival)
                   (tc-error/expr #:stx #'e
                                  #:return (or expected (ret -Void))
                                  "expected statically known index for heterogenous vector, but got ~a" (match e-t [(tc-result1: t) t]))]
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
         [v-ty
          (let ([arg-tys (list v-ty e-t (single-value #'val))])
            (tc/funapp #'op #'(v e val) (single-value #'op) arg-tys expected))]))]
    [(#%plain-app (~and op (~literal vector)) args:expr ...)
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
       [(or #f (tc-result1: _))
        (let ([arg-tys (map single-value (syntax->list #'(args ...)))])
            (tc/funapp #'op #'(args ...) (single-value #'op) arg-tys expected))
        #;#;
        (tc-error/expr "expected ~a, but got ~a" t (make-HeterogenousVector (map tc-expr/t (syntax->list #'(args ...)))))
        expected]
       [_ (int-err "bad expected: ~a" expected)])]
    ;; special case for `-' used like `sub1'
    [(#%plain-app (~and op (~literal -)) v (~and arg2 ((~literal quote) 1)))
     (add-typeof-expr #'arg2 (ret -Nat))
     (match-let ([(tc-result1: t) (single-value #'v)])
       (if (subtype t -ExactPositiveInteger)
           (ret -Nat)
           (tc/funapp #'op #'(v arg2) (single-value #'op) (list (ret t) (single-value #'arg2)) expected)))]
    ;; call-with-values
    [(#%plain-app call-with-values prod con)
     (match (tc/funapp #'prod #'() (single-value #'prod) null #f)
       [(tc-results: ts fs os)
        (tc/funapp #'con #'prod (single-value #'con) (map ret ts fs os) expected)])]
    ;; in eq? cases, call tc/eq
    [(#%plain-app eq?:comparator v1 v2)
     ;; make sure the whole expression is type correct
     (match* ((tc/funapp #'eq? #'(v1 v2) (single-value #'eq?) (map single-value (syntax->list #'(v1 v2))) expected)
              ;; check thn and els with the eq? info
              (tc/eq #'eq? #'v1 #'v2))
       [((tc-result1: t) (tc-result1: t* f o))
        (ret t f o)])]
    ;; special-case for not - flip the filters
    [(#%plain-app not arg)
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
    [(#%plain-app values arg) (single-value #'arg expected)]
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
    [(#%plain-app (letrec-values ([(lp) (#%plain-lambda args . body)]) lp*) . actuals)
     #:fail-unless expected #f 
     #:fail-unless (not (andmap type-annotation (syntax->list #'(lp . args)))) #f
     #:fail-unless (free-identifier=? #'lp #'lp*) #f
     (let-loop-check form #'lp #'actuals #'args #'body expected)]
    ;; special cases for classes
    [(#%plain-app make-object cl . args)     
     (check-do-make-object #'cl #'args #'() #'())]
    [(#%plain-app do-make-object cl (#%plain-app list . pos-args) (#%plain-app list (#%plain-app cons 'names named-args) ...))
     (check-do-make-object #'cl #'pos-args #'(names ...) #'(named-args ...))]
    [(#%plain-app (~literal map) f arg)
     (match (single-value #'arg)
       ;; if the argument is a ListDots
       [(tc-result1: (ListDots: t bound))
        
        (match (extend-tvars (list bound)
                 ;; just check that the function applies successfully to the element type
                 (tc/funapp #'f #'(arg) (tc-expr #'f) (list (ret t)) expected))
          [(tc-result1: t) (ret (make-ListDots t bound))]
          [(tc-results: ts)
           (tc-error/expr #:return (ret (Un))
                          "Expected one value, but got ~a" (-values ts))])]
       ;; otherwise, if it's not a ListDots, defer to the regular function typechecking
       [res
        (tc/funapp #'map #'(f arg) (single-value #'map) (list (tc-expr #'f) res) expected)])]
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
       ;(printf "calling list: ~a ~a~n" (syntax->datum #'args) expected)
       (match expected
         [(tc-result1: (Mu: var (Union: (or 
                                         (list (Pair: elem-ty (F: var)) (Value: '()))
                                         (list (Value: '()) (Pair: elem-ty (F: var)))))))
          ;(printf "special case 1 ~a~n" elem-ty)
          (for ([i (in-list (syntax->list #'args))])
               (tc-expr/check i (ret elem-ty)))
          expected]
         [(tc-result1: (app untuple (? (lambda (ts) (and ts (= (length (syntax->list #'args))
                                                               (length ts))))
                                       ts)))    
          ;(printf "special case 2 ~a~n" ts)
          (for ([ac (in-list (syntax->list #'args))]
                [exp (in-list ts)])
               (tc-expr/check ac (ret exp)))
          expected]
         [_
          ;(printf "not special case~n")
          (let ([tys (map tc-expr/t (syntax->list #'args))])
            (ret (apply -lst* tys)))]))]
    ;; special case for `list*'
    [(#%plain-app list* . args)
     (match-let* ([(list last tys-r ...) (reverse (map tc-expr/t (syntax->list #'args)))]
                  [tys (reverse tys-r)])
       (ret (foldr make-Pair last tys)))]
    ;; special case for `reverse' to propogate expected type info
    [(#%plain-app reverse arg)
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
          ;(printf "f dom: ~a ~a~n" (syntax->datum #'f) dom)
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


(define-syntax (handle-clauses stx)
  (syntax-parse stx
    [(_  (lsts ... arrs) f-stx args-stx pred infer t argtys expected)
     (with-syntax ([(vars ... a) (generate-temporaries #'(lsts ... arrs))])
       (syntax/loc stx
         (or (for/or ([vars lsts] ... [a arrs]
                      #:when (pred vars ... a))
               (let ([substitution (infer vars ... a)])
                 (and substitution
                      (tc/funapp1 f-stx args-stx (subst-all substitution a) argtys expected #:check #f))))
             (poly-fail t argtys #:name (and (identifier? f-stx) f-stx) #:expected expected))))]))

(define (tc/funapp f-stx args-stx ftype0 argtys expected)
  ;(syntax? syntax? tc-results? (listof tc-results?) (or/c #f tc-results?) . -> . tc-results?)
  (match* (ftype0 argtys)
    ;; we special-case this (no case-lambda) for improved error messages
    [((tc-result1: (and t (Function: (list (and a (arr: dom (Values: (list (Result: t-r lf-r lo-r) ...)) rest #f kws))))))
      argtys)
     (tc/funapp1 f-stx args-stx a argtys expected)]
    [((tc-result1: (and t (Function: (and arrs (list (arr: doms rngs rests (and drests #f) kws) ...)))))
      (and argtys (list (tc-result1: argtys-t) ...)))
     (or 
      ;; find the first function where the argument types match
      (for/first ([dom doms] [rng rngs] [rest rests] [a arrs]
                  #:when (subtypes/varargs argtys-t dom rest))
        ;; then typecheck here
        ;; we call the separate function so that we get the appropriate filters/objects
        (tc/funapp1 f-stx args-stx a argtys expected #:check #f))
      ;; if nothing matched, error
      (tc-error/expr 
       #:return (or expected (ret (Un)))
       (string-append "No function domains matched in function application:\n"
                      (domain-mismatches t doms rests drests rngs argtys-t #f #f))))]
    ;; any kind of dotted polymorphic function without mandatory keyword args
    [((tc-result1: (and t (PolyDots: 
                           (and vars (list fixed-vars ... dotted-var))
                           (Function: (list (and arrs (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...))) ...)))))
      (list (tc-result1: argtys-t) ...))
     (handle-clauses (doms rngs rests drests arrs) f-stx args-stx
                     ;; only try inference if the argument lengths are appropriate
                     (lambda (dom _ rest drest a) 
                       (cond [rest (<= (length dom) (length argtys))]
                             [drest (and (<= (length dom) (length argtys))
                                         (eq? dotted-var (cdr drest)))]
                             [else (= (length dom) (length argtys))]))
                     ;; only try to infer the free vars of the rng (which includes the vars in filters/objects)
                     ;; note that we have to use argtys-t here, since argtys is a list of tc-results
                     (lambda (dom rng rest drest a)
                       (cond 
                         [drest
                          (infer/dots fixed-vars dotted-var argtys-t dom (car drest) rng (fv rng) 
                                       #:expected (and expected (tc-results->values expected)))]
                         [rest 
                          (infer/vararg fixed-vars (list dotted-var) argtys-t dom rest rng
                                        (and expected (tc-results->values expected)))]
                         ;; no rest or drest
                         [else (infer fixed-vars (list dotted-var) argtys-t dom rng
                                      (and expected (tc-results->values expected)))]))
                     t argtys expected)]
    ;; regular polymorphic functions without dotted rest, and without mandatory keyword args
    [((tc-result1: 
       (and t
            (Poly: 
             vars 
             (Function: (list (and arrs (arr: doms rngs rests (and drests #f) (list (Keyword: _ _ #f) ...))) ...)))))
      (list (tc-result1: argtys-t) ...))
     (handle-clauses (doms rngs rests arrs) f-stx args-stx
                     ;; only try inference if the argument lengths are appropriate
                     (lambda (dom _ rest a) ((if rest <= =) (length dom) (length argtys)))
                     ;; only try to infer the free vars of the rng (which includes the vars in filters/objects)
                     ;; note that we have to use argtys-t here, since argtys is a list of tc-results
                     (lambda (dom rng rest a) (infer/vararg vars null argtys-t dom rest rng (and expected (tc-results->values expected))))
                     t argtys expected)]
    ;; procedural structs
    [((tc-result1: (and sty (Struct: _ _ _ (? Function? proc-ty) _ _ _ _ _))) _)
     (tc/funapp f-stx #`(#,(syntax/loc f-stx dummy) . #,args-stx) (ret proc-ty) (cons ftype0 argtys) expected)]
    ;; parameters are functions too
    [((tc-result1: (Param: in out)) (list)) (ret out)]
    [((tc-result1: (Param: in out)) (list (tc-result1: t)))
     (if (subtype t in) 
         (ret -Void true-filter)
         (tc-error/expr #:return (ret -Void true-filter)
                        "Wrong argument to parameter - expected ~a and got ~a" in t))]
    [((tc-result1: (Param: _ _)) _) 
     (tc-error/expr #:return (ret (Un))
                    "Wrong number of arguments to parameter - expected 0 or 1, got ~a"
                    (length argtys))]
    ;; resolve names, polymorphic apps, mu, etc
    [((tc-result1: (? needs-resolving? t) f o) _)
     (tc/funapp f-stx args-stx (ret (resolve-once t) f o) argtys expected)]
    ;; a union of functions can be applied if we can apply all of the elements
    [((tc-result1: (Union: (and ts (list (Function: _) ...)))) _)
     (ret (for/fold ([result (Un)]) ([fty ts])            
            (match (tc/funapp f-stx args-stx (ret fty) argtys expected)
              [(tc-result1: t) (Un result t)])))]
    ;; error type is a perfectly good fcn type
    [((tc-result1: (Error:)) _) (ret (make-Error))]
    ;; otherwise fail
    [((tc-result1: f-ty) _) 
     ;(printf "ft: ~a argt: ~a~n" ftype0 argtys)
     (tc-error/expr #:return (ret (Un))
                    "Cannot apply expression of type ~a, since it is not a function type" f-ty)]))


;; syntax? syntax? arr? (listof tc-results?) (or/c #f tc-results) [boolean?] -> tc-results?
(d/c (tc/funapp1 f-stx args-stx ftype0 argtys expected #:check [check? #t])
  ((syntax? syntax? arr? (c:listof tc-results?) (c:or/c #f tc-results?)) (#:check boolean?) . c:->* . tc-results?)
  ;(printf "got to here 0~a~n" args-stx)
  (match* (ftype0 argtys)
    ;; we check that all kw args are optional
    [((arr: dom (Values: (and results (list (Result: t-r f-r o-r) ...))) rest #f (and kws (list (Keyword: _ _ #f) ...)))
      (list (tc-result1: t-a phi-a o-a) ...))
     ;(printf "got to here 1~a~n" args-stx)
     (when check?
       (cond [(and (not rest) (not (= (length dom) (length t-a))))
              (tc-error/expr #:return (ret t-r)
                             "Wrong number of arguments, expected ~a and got ~a" (length dom) (length t-a))]
             [(and rest (< (length t-a) (length dom)))
              (tc-error/expr #:return (ret t-r)
                             "Wrong number of arguments, expected at least ~a and got ~a" (length dom) (length t-a))])
       (for ([dom-t (if rest (in-sequence-forever dom rest) (in-list dom))] 
             [a (in-list (syntax->list args-stx))]
             [arg-t (in-list t-a)])
         (parameterize ([current-orig-stx a]) (check-below arg-t dom-t))))
     ;(printf "got to here 2 ~a ~a ~a ~n" dom names o-a)
     (let* ([dom-count (length dom)]
            [arg-count (+ dom-count (if rest 1 0) (length kws))])
       (let-values
           ([(o-a t-a) (for/lists (os ts)
                         ([nm (in-range arg-count)]
                          [oa (in-sequence-forever (in-list o-a) (make-Empty))]
                          [ta (in-sequence-forever (in-list t-a) (Un))])
                         (values (if (>= nm dom-count) (make-Empty) oa)
                                 ta))])
         (define-values (t-r f-r o-r)
           (for/lists (t-r f-r o-r) 
             ([r (in-list results)])
             (open-Result r o-a t-a)))
         (ret t-r f-r o-r)))]
    [((arr: _ _ _ drest '()) _)
     (int-err "funapp with drest args ~a NYI" drest)]
    [((arr: _ _ _ _ kws) _)
     (int-err "funapp with keyword args ~a NYI" kws)]))

