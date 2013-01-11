#lang racket/base

(require "../utils/utils.rkt"
         (except-in (rep type-rep object-rep filter-rep) make-arr)
         (rename-in (types abbrev union utils printer filter-ops resolve)
                    [make-arr* make-arr])
         (utils tc-utils stxclass-util)
         syntax/stx (prefix-in c: racket/contract)
         syntax/parse racket/dict
         (env type-env-structs tvar-env type-name-env type-alias-env
              lexical-env index-env)
         racket/match
         "parse-classes.rkt"
         (for-template racket/base "../base-env/colon.rkt")
         ;; needed at this phase for tests
         "../base-env/colon.rkt"
         (prefix-in t: "../base-env/base-types-extra.rkt")
         (for-template (prefix-in t: "../base-env/base-types-extra.rkt")))

(define-struct poly (name vars) #:prefab)

(provide/cond-contract [parse-type (syntax? . c:-> . Type/c)]
                       [parse-type/id (syntax? c:any/c . c:-> . Type/c)]
                       [parse-tc-results (syntax? . c:-> . tc-results?)]
                       [parse-tc-results/id (syntax? c:any/c . c:-> . tc-results?)])

(provide star ddd/bound)
(define enable-mu-parsing (make-parameter #t))
(print-complex-filters? #t)

(define ((parse/id p) loc datum)
  #;(printf "parse-type/id id : ~a\n ty: ~a\n" (syntax-object->datum loc) (syntax-object->datum stx))
  (let* ([stx* (datum->syntax loc datum loc loc)])
    (p stx*)))


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
  ;(printf "parse-all-type: ~a \n" (syntax->datum stx))
  (syntax-parse stx #:literals (t:All)
    [((~and kw t:All) (vars:id ... v:id dd:ddd) . t)
     (when (check-duplicate-identifier (syntax->list #'(vars ... v)))
       (tc-error "All: duplicate type variable or index"))
     (let* ([vars (map syntax-e (syntax->list #'(vars ...)))]
            [v (syntax-e #'v)])
       (add-disappeared-use #'kw)
       (extend-indexes v
         (extend-tvars vars
           (make-PolyDots (append vars (list v)) (parse-all-body #'t)))))]
    [((~and kw t:All) (vars:id ...) . t)
     (when (check-duplicate-identifier (syntax->list #'(vars ...)))
       (tc-error "All: duplicate type variable"))
     (let* ([vars (map syntax-e (syntax->list #'(vars ...)))])
       (add-disappeared-use #'kw)
       (extend-tvars vars
         (make-Poly vars (parse-all-body #'t))))]
    [(t:All (_:id ...) _ _ _ ...) (tc-error "All: too many forms in body of All type")]
    [(t:All . rest) (tc-error "All: bad syntax")]))

(define-splicing-syntax-class keyword-tys
  (pattern (~seq k:keyword t:expr)
           #:attr Keyword (make-Keyword (syntax-e #'k) (parse-type #'t) #t))
  (pattern (~seq [k:keyword t:expr])
           #:attr Keyword (make-Keyword (syntax-e #'k) (parse-type #'t) #f)))

(define-syntax-class non-keyword-ty
  (pattern (k e:expr ...)
           #:when (not (keyword? (syntax->datum #'k))))
  (pattern t:expr
           #:when (and (not (keyword? (syntax->datum #'t)))
                       (not (syntax->list #'t)))))

(define-syntax-class path-elem
  #:description "path element"
  #:literals (car cdr)
  (pattern car
           #:attr pe (make-CarPE))
  (pattern cdr
           #:attr pe (make-CdrPE)))

(define-splicing-syntax-class simple-latent-filter
  #:description "latent filter"
  (pattern (~seq t:expr (~describe "@" (~datum @)) pe:path-elem ...)
           #:attr type (parse-type #'t)
           #:attr path (attribute pe.pe))
  (pattern t:expr
           #:attr type (parse-type #'t)
           #:attr path '()))

(define-syntax-class prop
  #:attributes (prop)
  (pattern (~literal Top) #:attr prop -top)
  (pattern (~literal Bot) #:attr prop -bot)
  (pattern (t:expr (~describe "@" (~datum @)) pe:path-elem ... i:nat)
           #:attr prop (-filter (parse-type #'t) (syntax-e #'i) (attribute pe.pe)))
  (pattern ((~datum !) t:expr (~describe "@" (~datum @)) pe:path-elem ... i:nat)
           #:attr prop (-not-filter (parse-type #'t) (syntax-e #'i) (attribute pe.pe)))
  (pattern ((~literal and) p:prop ...)
           #:attr prop (apply -and (attribute p.prop)))
  (pattern ((~literal or) p:prop ...)
           #:attr prop (apply -or (attribute p.prop)))
  (pattern ((~literal implies) p1:prop p2:prop)
           #:attr prop (-imp (attribute p1.prop) (attribute p2.prop))))

(define-syntax-class object
  #:attributes (object)
  (pattern e:expr
           #:attr object -no-obj))

(define-splicing-syntax-class full-latent
  #:description "latent propositions and object"
  (pattern (~seq (~optional (~seq #:+ p+:prop ...))
                 (~optional (~seq #:- p-:prop ...))
                 (~optional (~seq #:object o:object)))
           #:attr positive (if (attribute p+.prop)
                               (apply -and (attribute p+.prop))
                               -top)
           #:attr negative (if (attribute p-.prop)
                               (apply -and (attribute p-.prop))
                               -top)
           #:attr object (if (attribute o.object)
                             (attribute o.object)
                             -no-obj)))

(define (parse-type stx)
  (parameterize ([current-orig-stx stx])
    (syntax-parse
        stx
      #:literals (t:Class t:Refinement t:Instance t:List t:List* cons t:pred t:-> : case-lambda t:case->
                  t:Rec t:U t:All t:Opaque t:Parameter t:Vector quote t:Struct)
      [t
       #:declare t (3d Type?)
       (attribute t.datum)]
      [(fst . rst)
       #:fail-unless (not (syntax->list #'rst)) #f
       (-pair (parse-type #'fst) (parse-type #'rst))]
      [((~and kw t:Class) (pos-args ...) ([fname fty . rest] ...) ([mname mty] ...))
       (add-disappeared-use #'kw)
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
       (add-disappeared-use #'kw)
       (match (lookup-type/lexical #'p?)
         [(and t (Function: (list (arr: (list dom) _ #f #f '()))))
          (make-Refinement dom #'p? (syntax-local-certifier))]
         [t (tc-error "cannot declare refinement for non-predicate ~a" t)])]
      [((~and kw t:Struct) t)
       (add-disappeared-use #'kw)
       (let ([v (parse-type #'t)])
         (match (resolve v)
           [(and s Struct?) (make-StructTop s)]
           [_ (tc-error/delayed "Argument to Struct must be a structure type, got ~a" v)
              (make-Instance (Un))]))]
      [((~and kw t:Instance) t)
       (add-disappeared-use #'kw)
       (let ([v (parse-type #'t)])
         (if (not (or (Mu? v) (Class? v) (Union? v) (Error? v)))
             (begin (tc-error/delayed "Argument to Instance must be a class type, got ~a" v)
                    (make-Instance (Un)))
             (make-Instance v)))]
      [((~and kw t:List) ts ...)
       (add-disappeared-use #'kw)
       (parse-list-type stx)]
      [((~and kw t:List*) ts ... t)
       (add-disappeared-use #'kw)
       (-Tuple* (map parse-type (syntax->list #'(ts ...))) (parse-type #'t))]
      [((~and kw t:Vector) ts ...)
       (add-disappeared-use #'kw)
       (make-HeterogeneousVector (map parse-type (syntax->list #'(ts ...))))]
      [((~and kw cons) fst rst)
       (add-disappeared-use #'kw)
       (-pair (parse-type #'fst) (parse-type #'rst))]
      [((~and kw t:pred) t)
       (add-disappeared-use #'kw)
       (make-pred-ty (parse-type #'t))]
      [((~and kw (~or case-lambda t:case->)) tys ...)
       (add-disappeared-use #'kw)
       (make-Function
        (for/list ([ty (syntax->list #'(tys ...))])
          (let ([t (parse-type ty)])
            (match t
              [(Function: (list arr)) arr]
              [_ (tc-error/stx
                  ty
                  "Component of case-lambda type was not a function clause")]))))]
      #;[((~and kw t:Vectorof) t)
       (add-disappeared-use #'kw)
       (make-Vector (parse-type #'t))]
      [((~and kw t:Rec) x:id t)
       #:fail-unless (enable-mu-parsing) "Recursive types not allowed"
       (let* ([var (syntax-e #'x)]
              [tvar (make-F var)])
         (add-disappeared-use #'kw)
         (extend-tvars (list var)
           (let ([t* (parse-type #'t)])
             ;; is t in a productive position?
             (define productive
               (let loop ((ty t*))
                 (match ty
                  [(Union: elems) (andmap loop elems)]
                  [(F: _) (not (equal? ty tvar))]
                  [(App: rator rands stx)
                   (loop (resolve-app rator rands stx))]
                  [(Mu: _ body) (loop body)]
                  [(Poly: names body) (loop body)]
                  [(PolyDots: names body) (loop body)]
                  [else #t])))
             (unless productive
               (tc-error/stx
                stx
                "Recursive types are not allowed directly inside their definition"))
             (if (memq var (fv t*))
                 (make-Mu var t*)
                 t*))))]
      [((~and kw t:U) ts ...)
       (add-disappeared-use #'kw)
       (apply Un (map parse-type (syntax->list #'(ts ...))))]
      [((~and kw quote) (t1 . t2))
       (add-disappeared-use #'kw)
       (-pair (parse-type #'(quote t1)) (parse-type #'(quote t2)))]
      [((~and kw quote) t)
       (add-disappeared-use #'kw)
       (-val (syntax->datum #'t))]
      [((~and kw t:All) . rest)
       (parse-all-type stx parse-type)]
      [((~and kw t:Opaque) p?)
       (add-disappeared-use #'kw)
       (make-Opaque #'p? (syntax-local-certifier))]
      [((~and kw t:Parameter) t)
       (let ([ty (parse-type #'t)])
         (add-disappeared-use #'kw)
         (-Param ty ty))]
      [((~and kw t:Parameter) t1 t2)
       (add-disappeared-use #'kw)
       (-Param (parse-type #'t1) (parse-type #'t2))]
      ;; curried function notation
      [((~and dom:non-keyword-ty (~not t:->)) ...
        (~and kw t:->)
        (~and (~seq rest-dom ...) (~seq (~or _ (~between t:-> 1 +inf.0)) ...)))
       (add-disappeared-use #'kw)
       (let ([doms (for/list ([d (syntax->list #'(dom ...))])
                    (parse-type d))])
         (make-Function
          (list (make-arr
                 doms
                 (parse-type (syntax/loc stx (rest-dom ...)))))))]
      [(dom ... (~and kw t:->) rng : latent:full-latent)
       (add-disappeared-use #'kw)
       ;; use parse-type instead of parse-values-type because we need to add the filters from the pred-ty
       (->* (map parse-type (syntax->list #'(dom ...)))
            (parse-type #'rng)
            : (-FS (attribute latent.positive) (attribute latent.negative))
            : (attribute latent.object))]
      [(dom (~and kw t:->) rng : ~! latent:simple-latent-filter)
       (add-disappeared-use #'kw)
       ;; use parse-type instead of parse-values-type because we need to add the filters from the pred-ty
       (make-pred-ty (list (parse-type #'dom)) (parse-type #'rng) (attribute latent.type) 0 (attribute latent.path))]
      [(dom:non-keyword-ty ... rest:non-keyword-ty ddd:star kws:keyword-tys ... (~and kw t:->) rng)
       (add-disappeared-use #'kw)
       (make-Function
        (list (make-arr
               (map parse-type (syntax->list #'(dom ...)))
               (parse-values-type #'rng)
               #:rest (parse-type #'rest)
               #:kws (attribute kws.Keyword))))]
      [(dom:non-keyword-ty ... rest:non-keyword-ty :ddd/bound (~and kw t:->) rng)
       (add-disappeared-use #'kw)
       (let* ([bnd (syntax-e #'bound)])
         (unless (bound-index? bnd)
           (tc-error/stx #'bound
                         "Used a type variable (~a) not bound with ... as a bound on a ..."
                         bnd))
         (make-Function
          (list
           (make-arr-dots (map parse-type (syntax->list #'(dom ...)))
                          (parse-values-type #'rng)
                          (extend-tvars (list bnd)
                            (parse-type #'rest))
                          bnd))))]
      [(dom:non-keyword-ty ... rest:non-keyword-ty _:ddd (~and kw t:->) rng)
       (add-disappeared-use #'kw)
       (let ([var (infer-index stx)])
         (make-Function
          (list
           (make-arr-dots (map parse-type (syntax->list #'(dom ...)))
                          (parse-values-type #'rng)
                          (extend-tvars (list var) (parse-type #'rest))
                          var))))]
      #| ;; has to be below the previous one
     [(dom:expr ... (~and kw t:->) rng)
      (add-disappeared-use #'kw)
      (->* (map parse-type (syntax->list #'(dom ...)))
           (parse-values-type #'rng))]     |#
      ;; use expr to rule out keywords
      [(dom:non-keyword-ty ... kws:keyword-tys ... (~and kw t:->) rng)
       (add-disappeared-use #'kw)
      (let ([doms (for/list ([d (syntax->list #'(dom ...))])
                    (parse-type d))])
         (make-Function
          (list (make-arr
                 doms
                 (parse-values-type #'rng)
                 #:kws (attribute kws.Keyword)))))]

      [id:identifier
       (cond
         ;; if it's a type variable, we just produce the corresponding reference (which is in the HT)
         [(bound-tvar? (syntax-e #'id))
          (lookup-tvar (syntax-e #'id))]
         ;; if it was in current-indexes, produce a better error msg
         [(bound-index? (syntax-e #'id))
          (tc-error
           "Type variable ~a must be used with ..."
           (syntax-e #'id))]
         ;; if it's a type alias, we expand it (the expanded type is stored in the HT)
         [(lookup-type-alias #'id parse-type (lambda () #f))
          =>
          (lambda (t)
            ;(printf "found a type alias ~a\n" #'id)
            (add-disappeared-use #'id)
            t)]
         ;; if it's a type name, we just use the name
         [(lookup-type-name #'id (lambda () #f))
          (add-disappeared-use #'id)
          ;(printf "found a type name ~a\n" #'id)
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
         (resolve-app-check-error rator args stx)
         (match rator
           [(Name: _) (make-App rator args stx)]
           [(Poly: _ _) (instantiate-poly rator args)]
           [(Mu: _ _) (loop (unfold rator) args)]
           [(Error:) Err]
           [_ Err]))]
      [t:atom
       (-val (syntax-e #'t))]
      [_ (tc-error "not a valid type: ~a" (syntax->datum stx))])))

(define (parse-list-type stx)
  (parameterize ([current-orig-stx stx])
    (syntax-parse stx #:literals (t:List)
      [((~and kw t:List) tys ... dty :ddd/bound)
       (add-disappeared-use #'kw)
       (let ([var (syntax-e #'bound)])
         (unless (bound-index? var)
           (if (bound-tvar? var)
               (tc-error/stx #'bound "Used a type variable (~a) not bound with ... as a bound on a ..." var)
               (tc-error/stx #'bound "Type variable ~a is unbound" var)))
         (-Tuple* (map parse-type (syntax->list #'(tys ...)))
                  (make-ListDots
                   (extend-tvars (list var)
                     (parse-type #'dty))
                   var)))]
      [((~and kw t:List) tys ... dty _:ddd)
       (add-disappeared-use #'kw)
       (let ([var (infer-index stx)])
         (-Tuple* (map parse-type (syntax->list #'(tys ...)))
                    (make-ListDots
                     (extend-tvars (list var)
                       (parse-type #'dty))
                     var)))]
      [((~and kw t:List) tys ...)
       (add-disappeared-use #'kw)
       (-Tuple (map parse-type (syntax->list #'(tys ...))))])))

(define (parse-values-type stx)
  (parameterize ([current-orig-stx stx])
    (syntax-parse stx #:literals (values t:Values t:All)
      [((~and kw (~or t:Values values)) tys ... dty :ddd/bound)
       (add-disappeared-use #'kw)
       (let ([var (syntax-e #'bound)])
         (unless (bound-index? var)
           (if (bound-tvar? var)
               (tc-error/stx #'bound "Used a type variable (~a) not bound with ... as a bound on a ..." var)
               (tc-error/stx #'bound "Type variable ~a is unbound" var)))
         (-values-dots (map parse-type (syntax->list #'(tys ...)))
                       (extend-tvars (list var)
                         (parse-type #'dty))
                       var))]
      [((~and kw (~or t:Values values)) tys ... dty _:ddd)
       (add-disappeared-use #'kw)
       (let ([var (infer-index stx)])
         (-values-dots (map parse-type (syntax->list #'(tys ...)))
                       (extend-tvars (list var)
                         (parse-type #'dty))
                       var))]
      [((~and kw (~or t:Values values)) tys ...)
       (add-disappeared-use #'kw)
       (-values (map parse-type (syntax->list #'(tys ...))))]
      [t
       (-values (list (parse-type #'t)))])))

(define (parse-tc-results stx)
  (syntax-parse stx #:literals (values)
    [((~and kw values) t ...)
     (add-disappeared-use #'kw)
     (ret (map parse-type (syntax->list #'(t ...)))
          (map (lambda (x) (make-NoFilter)) (syntax->list #'(t ...)))
          (map (lambda (x) (make-NoObject)) (syntax->list #'(t ...))))]
    [t (ret (parse-type #'t) (make-NoFilter) (make-NoObject))]))

(define parse-tc-results/id (parse/id parse-tc-results))

(define parse-type/id (parse/id parse-type))
