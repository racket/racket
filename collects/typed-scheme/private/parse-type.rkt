#lang scheme/base

(require "../utils/utils.rkt"
	 (except-in (rep type-rep) make-arr)
         (rename-in (types convenience union utils) [make-arr* make-arr])
         (utils tc-utils stxclass-util)
         syntax/stx (prefix-in c: scheme/contract)
         syntax/parse
         (env type-environments type-name-env type-alias-env lexical-env)         
         scheme/match unstable/debug
         (for-template scheme/base "colon.ss")
         ;; needed at this phase for tests
         (combine-in (prefix-in t: "base-types-extra.ss") "colon.ss")
         (for-template (prefix-in t: "base-types-extra.ss")))

(define-struct poly (name vars) #:prefab)

(p/c [parse-type (syntax? . c:-> . Type/c)]
     [parse-type/id (syntax? c:any/c . c:-> . Type/c)] 
     [parse-tc-results (syntax? . c:-> . tc-results?)] 
     [parse-tc-results/id (syntax? c:any/c . c:-> . tc-results?)])

(provide star ddd/bound)
(define enable-mu-parsing (make-parameter #t))

(define ((parse/id p) loc datum)
  #;(printf "parse-type/id id : ~a~n ty: ~a~n" (syntax-object->datum loc) (syntax-object->datum stx))
  (let* ([stx* (datum->syntax loc datum loc loc)])
    (p stx*)))


(define-syntax-class star
  #:description "*"
  (pattern star:id
           #:fail-unless (eq? '* (syntax-e #'star)) "missing *")
  (pattern star:id
           #:fail-unless (eq? '...* (syntax-e #'star)) "missing ...*"))

(define-syntax-class ddd
  #:description "..."
  (pattern ddd:id
           #:fail-unless (eq? '... (syntax-e #'ddd)) "missing ..."))

(define-splicing-syntax-class ddd/bound
  #:description "... followed by variable name"
  #:attributes (bound)
  (pattern i:id
           #:attr s (symbol->string (syntax-e #'i))
           #:fail-unless ((string-length (attribute s)) . > . 3) #f
           #:fail-unless (equal? "..." (substring (attribute s) 0 3)) "missing ..."
           #:attr bound (datum->syntax #'i (string->symbol (substring (attribute s) 3)) #'i #'i))
  (pattern (~seq _:ddd bound:id)))

(define (parse-all-body s)
  (syntax-parse s
    [(ty)
     (parse-type #'ty)]
    [(x ...)
     #:fail-unless (= 1 (length 
			 (for/list ([i (syntax->list #'(x ...))]
				    #:when (and (identifier? i)
						(free-identifier=? i #'t:->)))
				   i))) 
     #f
     (parse-type s)]))

(define (parse-all-type stx parse-type)
  ;(printf "parse-all-type: ~a ~n" (syntax->datum stx))
  (syntax-parse stx #:literals (t:All)
    [((~and kw t:All) (vars:id ... v:id dd:ddd) . t)
     (let* ([vars (map syntax-e (syntax->list #'(vars ...)))]
            [tvars (map make-F vars)]
            [v (syntax-e #'v)]
            [tv (make-Dotted (make-F v))])
       (add-type-name-reference #'kw)
       (parameterize ([current-tvars (extend-env (cons v vars) (cons tv tvars) (current-tvars))])
         (make-PolyDots (append vars (list v)) (parse-all-body #'t))))]
    [((~and kw t:All) (vars:id ...) . t) 
     (let* ([vars (map syntax-e (syntax->list #'(vars ...)))]
            [tvars (map make-F vars)])
       (add-type-name-reference #'kw)
       (parameterize ([current-tvars (extend-env vars tvars (current-tvars))])
         (make-Poly vars (parse-all-body #'t))))]
    [(t:All (_:id ...) _ _ _ ...) (tc-error "All: too many forms in body of All type")]
    [(t:All . rest) (tc-error "All: bad syntax")]))

(define-splicing-syntax-class keyword-tys
  (pattern (~seq k:keyword t:expr)
           #:attr Keyword (make-Keyword (syntax-e #'k) (parse-type #'t) #t))
  (pattern (~seq [k:keyword t:expr])
           #:attr Keyword (make-Keyword (syntax-e #'k) (parse-type #'t) #f)))

(define-syntax-class path-elem
  #:description "path element"
  #:literals (car cdr)
  (pattern car
           #:attr pe (make-CarPE))
  (pattern cdr
           #:attr pe (make-CdrPE)))

(define-splicing-syntax-class latent-filter
  #:description "latent filter"  
  (pattern (~seq t:expr (~describe "@" (~datum @)) pe:path-elem ...)
           #:attr type (parse-type #'t)
           #:attr path (attribute pe.pe))
  (pattern t:expr
           #:attr type (parse-type #'t)
           #:attr path '()))

(define (parse-type stx)    
  (parameterize ([current-orig-stx stx])    
    (syntax-parse
        stx
      #:literals (t:Class t:Refinement t:Instance t:List cons t:pred t:-> : case-lambda
                          t:Rec t:U t:All t:Opaque t:Parameter t:Vector quote)
      [t
       #:declare t (3d Type?)
       (attribute t.datum)]
      [(fst . rst)
       #:fail-unless (not (syntax->list #'rst)) #f
       (-pair (parse-type #'fst) (parse-type #'rst))]
      [((~and kw t:Class) (pos-args ...) ([fname fty . rest] ...) ([mname mty] ...))
       (add-type-name-reference #'kw)
       (make-Class
        (map parse-type (syntax->list #'(pos-args ...)))
        (map list
             (map syntax-e (syntax->list #'(fname ...)))
             (map parse-type (syntax->list #'(fty ...)))
             (map (lambda (e) (syntax-case e ()
                                [(#t) #t]
                                [_ #f]))
                  (syntax->list #'(rest ...))))
        (map list
             (map syntax-e (syntax->list #'(mname ...)))
             (map parse-type (syntax->list #'(mty ...)))))]
      [((~and kw t:Refinement) p?:id)
       (add-type-name-reference #'kw)
       (match (lookup-type/lexical #'p?)
         [(and t (Function: (list (arr: (list dom) _ #f #f '()))))
          (make-Refinement dom #'p? (syntax-local-certifier))]
         [t (tc-error "cannot declare refinement for non-predicate ~a" t)])]
      [((~and kw t:Instance) t)
       (add-type-name-reference #'kw)
       (let ([v (parse-type #'t)])
         (if (not (or (Mu? v) (Class? v) (Union? v) (Error? v)))
             (begin (tc-error/delayed "Argument to Instance must be a class type, got ~a" v)
                    (make-Instance (Un)))
             (make-Instance v)))]
      [((~and kw t:List) ts ...)
       (add-type-name-reference #'kw)
       (-Tuple (map parse-type (syntax->list #'(ts ...))))]
      [((~and kw t:Vector) ts ...)
       (add-type-name-reference #'kw)
       (make-HeterogenousVector (map parse-type (syntax->list #'(ts ...))))]
      [((~and kw cons) fst rst)
       (add-type-name-reference #'kw)
       (-pair (parse-type #'fst) (parse-type #'rst))]
      [((~and kw t:pred) t) 
       (add-type-name-reference #'kw)
       (make-pred-ty (parse-type #'t))]     
      [((~and kw case-lambda) tys ...)
       (add-type-name-reference #'kw)
       (make-Function 
        (for/list ([ty (syntax->list #'(tys ...))])
          (let ([t (parse-type ty)])
            (match t
              [(Function: (list arr)) arr]
              [_ (tc-error/stx 
                  ty
                  "Component of case-lambda type was not a function clause")]))))]
      #;[((~and kw t:Vectorof) t)
       (add-type-name-reference #'kw)
       (make-Vector (parse-type #'t))]
      [((~and kw t:Rec) x:id t)
       #:fail-unless (enable-mu-parsing) "Recursive types not allowed"
       (let* ([var (syntax-e #'x)]
              [tvar (make-F var)])
         (add-type-name-reference #'kw)
         (parameterize ([current-tvars (extend-env 
                                        (list var) 
                                        (list tvar)
                                        (current-tvars))])
           (let ([t (parse-type #'t)])
             (if (memq var (fv t))
                 (make-Mu var t)
                 t))))]
      [((~and kw t:U) ts ...)
       (add-type-name-reference #'kw)
       (apply Un (map parse-type (syntax->list #'(ts ...))))]
      [((~and kw quote) (t1 . t2))
       (add-type-name-reference #'kw)
       (-pair (parse-type #'(quote t1)) (parse-type #'(quote t2)))]
      [((~and kw quote) t)
       (add-type-name-reference #'kw)
       (-val (syntax->datum #'t))]     
      [((~and kw t:All) . rest)     
       (parse-all-type stx parse-type)]
      [((~and kw t:Opaque) p?) 
       (add-type-name-reference #'kw)
       (make-Opaque #'p? (syntax-local-certifier))]
      [((~and kw t:Parameter) t) 
       (let ([ty (parse-type #'t)])
         (add-type-name-reference #'kw)
         (-Param ty ty))]
      [((~and kw t:Parameter) t1 t2)      
       (add-type-name-reference #'kw)
       (-Param (parse-type #'t1) (parse-type #'t2))]
      ;; function types
      [(dom (~and kw t:->) rng : ~! latent:latent-filter)
       (add-type-name-reference #'kw)
       ;; use parse-type instead of parse-values-type because we need to add the filters from the pred-ty
       (make-pred-ty (list (parse-type #'dom)) (parse-type #'rng) (attribute latent.type) 0 (attribute latent.path))]
      [(dom:expr ... rest:expr ddd:star kws:keyword-tys ... (~and kw t:->) rng)
       (add-type-name-reference #'kw)
       (make-Function
        (list (make-arr
               (map parse-type (syntax->list #'(dom ...)))
               (parse-values-type #'rng)
               #:rest (parse-type #'rest)
               #:kws (attribute kws.Keyword))))]
      [(dom:expr ... rest:expr :ddd/bound (~and kw t:->) rng)
       (add-type-name-reference #'kw)
       (let ([var (lookup (current-tvars) (syntax-e #'bound) (lambda (_) #f))])
         (if (not (Dotted? var))
             (tc-error/stx #'bound 
                           "Used a type variable (~a) not bound with ... as a bound on a ..."
                           (syntax-e #'bound))
             (make-Function
              (list
               (make-arr-dots (map parse-type (syntax->list #'(dom ...)))
                              (parse-values-type #'rng)
                              (parameterize 
                                  ([current-tvars 
                                    (extend-env 
                                     (list (syntax-e #'bound)) 
                                     (list (make-DottedBoth (make-F (syntax-e #'bound))))
                                     (current-tvars))])
                                (parse-type #'rest))
                              (syntax-e #'bound))))))]
      [(dom:expr ... rest:expr _:ddd (~and kw t:->) rng)
       (add-type-name-reference #'kw)
       (let ([bounds (filter (compose Dotted? cdr)
                             (env-keys+vals (current-tvars)))])
         (when (null? bounds)
           (tc-error/stx stx "No type variable bound with ... in scope for ... type"))
         (unless (null? (cdr bounds))
           (tc-error/stx stx "Cannot infer bound for ... type"))
         (match-let ([(cons var (struct Dotted (t))) (car bounds)])
           (make-Function
            (list
             (make-arr-dots (map parse-type (syntax->list #'(dom ...)))
                            (parse-values-type #'rng)
                            (parameterize ([current-tvars 
                                            (extend-env (list var)
                                                        (list (make-DottedBoth t))
                                                        (current-tvars))])
                              (parse-type #'rest))
                            var)))))]
      #| ;; has to be below the previous one
     [(dom:expr ... (~and kw t:->) rng) 
      (add-type-name-reference #'kw)
      (->* (map parse-type (syntax->list #'(dom ...)))
           (parse-values-type #'rng))]     |#
      ;; use expr to rule out keywords
      [(dom:expr ... kws:keyword-tys ... (~and kw t:->) rng)
       (add-type-name-reference #'kw)
       (make-Function
        (list (make-arr
               (map parse-type (syntax->list #'(dom ...)))
               (parse-values-type #'rng)
               #:kws (attribute kws.Keyword))))]
      
      [id:identifier
       (cond 
         ;; if it's a type variable, we just produce the corresponding reference (which is in the HT)
         [(lookup (current-tvars) (syntax-e #'id) (lambda (_) #f))
          =>
          (lambda (e) (cond [(DottedBoth? e) (Dotted-t e)]
                            [(Dotted? e)
                             (tc-error 
                              "Type variable ~a must be used with ..."
                              (syntax-e #'id))]
                            [else e]))]
         ;; if it's a type alias, we expand it (the expanded type is stored in the HT)
         [(lookup-type-alias #'id parse-type (lambda () #f))
          =>
          (lambda (t)
            ;(printf "found a type alias ~a~n" #'id)
            (add-type-name-reference #'id)
            t)]
         ;; if it's a type name, we just use the name
         [(lookup-type-name #'id (lambda () #f))
          (add-type-name-reference #'id)
          ;(printf "found a type name ~a~n" #'id)
          (make-Name #'id)]
         [(free-identifier=? #'id #'t:->)
          (tc-error/delayed "Incorrect use of -> type constructor")
          Err]
         [else
          (tc-error/delayed 
           "Unbound type name ~a"
           (syntax-e #'id))
          Err])]      
      [(t:Opaque . rest)
       (tc-error "Opaque: bad syntax")]
      [(t:U . rest)
       (tc-error "Union: bad syntax")]
      #;[(t:Vectorof . rest)
       (tc-error "Vectorof: bad syntax")]
      [((~and (~datum mu) t:Rec) . rest) 
       (tc-error "mu: bad syntax")]
      [(t:Rec . rest) 
       (tc-error "Rec: bad syntax")]
      [(t ... t:-> . rest)
       (tc-error "->: bad syntax")]
      [(id arg args ...)
       (let loop 
         ([rator (parse-type #'id)]
          [args (map parse-type (syntax->list #'(arg args ...)))])
         (match rator
           [(Name: n)
            (when (and (current-poly-struct) 
                       (free-identifier=? n (poly-name (current-poly-struct)))
                       (not (andmap type-equal? args (poly-vars (current-poly-struct)))))
              (tc-error "Structure type constructor ~a applied to non-regular arguments ~a" rator args))
            (make-App rator args stx)]
           [(Poly: ns _)
            (unless (= (length args) (length ns))
              (tc-error "Wrong number of arguments to type ~a, expected ~a but got ~a" rator (length ns) (length args)))
            (instantiate-poly rator args)]
           [(Mu: _ _) (loop (unfold rator) args)]
           [(Error:) Err]
           [_ (tc-error/delayed "Type ~a cannot be applied, arguments were: ~a" rator args)
              Err]))
       #;
       (let ([ty (parse-type #'id)])
         #;(printf "ty is ~a" ty)
         (unless (Poly? ty)
           (tc-error "not a polymorphic type: ~a" (syntax-e #'id)))
         (unless (= (length (syntax->list #'(arg args ...))) (Poly-n ty))
           (tc-error "wrong number of arguments to type constructor ~a: expected ~a, got ~a" 
                     (syntax-e #'id)
                     (Poly-n ty)
                     (length (syntax->list #'(arg args ...)))))
         (instantiate-poly ty (map parse-type (syntax->list #'(arg args ...)))))]
      [t:atom
       (-val (syntax-e #'t))]
      [_ (tc-error "not a valid type: ~a" (syntax->datum stx))])))

(define (parse-values-type stx)
  (parameterize ([current-orig-stx stx])        
    (syntax-parse stx #:literals (values t:All)
      [((~and kw values) tys ... dty :ddd/bound)
       (add-type-name-reference #'kw)
       (let ([var (lookup (current-tvars) (syntax-e #'bound) (lambda (_) #f))])
         (if (not (Dotted? var))
             (tc-error/stx #'bound "Used a type variable (~a) not bound with ... as a bound on a ..." (syntax-e #'bound))             
             (make-ValuesDots (map parse-type (syntax->list #'(tys ...)))
                              (parameterize ([current-tvars (extend-env (list (syntax-e #'bound)) 
                                                                        (list (make-DottedBoth (make-F (syntax-e #'bound))))
                                                                        (current-tvars))])
                                (parse-type #'dty))
                              (syntax-e #'bound))))]
      [((~and kw values) tys ... dty _:ddd)
       (add-type-name-reference #'kw)
       (let ([bounds (filter (compose Dotted? cdr) (env-keys+vals (current-tvars)))])
         (when (null? bounds)
           (tc-error/stx stx "No type variable bound with ... in scope for ... type"))
         (unless (null? (cdr bounds))
           (tc-error/stx stx "Cannot infer bound for ... type"))
         (match-let ([(cons var (struct Dotted (t))) (car bounds)])
           (make-ValuesDots (map parse-type (syntax->list #'(tys ...)))
                            (parameterize ([current-tvars (extend-env (list var) 
                                                                      (list (make-DottedBoth t))
                                                                      (current-tvars))])
                              (parse-type #'dty))
                            var)))]
      [((~and kw values) tys ...) 
       (add-type-name-reference #'kw)
       (-values (map parse-type (syntax->list #'(tys ...))))]
      [t
       (-values (list (parse-type #'t)))])))

(define (parse-tc-results stx)
  (syntax-parse stx #:literals (values)
    [((~and kw values) t ...)
     (add-type-name-reference #'kw)
     (ret (map parse-type (syntax->list #'(t ...))) 
          (map (lambda (x) (make-NoFilter)) (syntax->list #'(t ...)))
          (map (lambda (x) (make-NoObject)) (syntax->list #'(t ...))))]
    [t (ret (parse-type #'t) (make-NoFilter) (make-NoObject))]))

(define parse-tc-results/id (parse/id parse-tc-results))

(define parse-type/id (parse/id parse-type))

