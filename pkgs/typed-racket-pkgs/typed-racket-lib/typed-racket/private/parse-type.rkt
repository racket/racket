#lang racket/base

;; This module provides functions for parsing types written by the user

(require "../utils/utils.rkt"
         (except-in (rep type-rep object-rep) make-arr)
         (rename-in (types abbrev union utils filter-ops resolve
                           classes subtype)
                    [make-arr* make-arr])
         (utils tc-utils stxclass-util literal-syntax-class)
         syntax/stx (prefix-in c: (contract-req))
         syntax/parse unstable/sequence
         (env tvar-env type-name-env type-alias-env mvar-env
              lexical-env index-env row-constraint-env)
         (only-in racket/list flatten)
         racket/dict
         racket/promise
         racket/format
         racket/match
         racket/syntax
         (only-in unstable/list check-duplicate)
         "parse-classes.rkt"
         (for-label
           (except-in racket/base case-lambda)
           "../base-env/colon.rkt"
           "../base-env/base-types-extra.rkt"
           ;; match on the `case-lambda` binding in the TR primitives
           ;; rather than the one from Racket, which is no longer bound
           ;; in most TR modules.
           (only-in "../base-env/case-lambda.rkt" case-lambda)))

(provide/cond-contract ;; Parse the given syntax as a type
                       [parse-type (syntax? . c:-> . Type/c)]
                       ;; Parse the given identifier using the lexical
                       ;; context of the given syntax object
                       [parse-type/id (syntax? c:any/c . c:-> . Type/c)]
                       [parse-tc-results (syntax? . c:-> . tc-results/c)]
                       [parse-literal-alls (syntax? . c:-> . (c:listof (c:or/c (c:listof identifier?) (c:list/c (c:listof identifier?) identifier?))))])

(provide star ddd/bound
         current-referenced-aliases
         current-referenced-class-parents
         current-type-alias-name)

;; current-type-alias-name : Parameter<(Option Id)>
;; This parameter stores the name of the type alias that is
;; being parsed (set in type-alias-helper.rkt), #f if the
;; parsing is not for a type alias
(define current-type-alias-name (make-parameter #f))

;; current-referenced-aliases : Parameter<Option<Box<List<Id>>>>
;; This parameter is used to coordinate with the type-checker to determine
;; if a type alias should be recursive or not
;;
;; interp. the argument is #f if not checking for type aliases.
;;         Otherwise, it should be a box containing a list of
;;         identifiers (i.e., type aliases in the syntax)
(define current-referenced-aliases (make-parameter #f))

;; current-referenced-class-parents : Parameter<Option<Box<List<Id>>>>
;; This parameter is used to coordinate with the type-checker about
;; the dependency structure of class types using #:implements
;;
;; interp. same as above
(define current-referenced-class-parents (make-parameter #f))

;; current-arities : Parameter<(Listof Natural)>
;; Represents the stack of function arities in the potentially
;; nested function type being parsed. The stack does not include the
;; innermost function (since syntax classes can check that locally)
(define current-arities (make-parameter null))

(define-syntax-rule (with-arity arity e ...)
  (parameterize ([current-arities (cons arity (current-arities))])
    e ...))

(define-literal-syntax-class #:for-label car)
(define-literal-syntax-class #:for-label cdr)
(define-literal-syntax-class #:for-label colon^ (:))
(define-literal-syntax-class #:for-label quote)
(define-literal-syntax-class #:for-label cons)
(define-literal-syntax-class #:for-label Class)
(define-literal-syntax-class #:for-label Object)
(define-literal-syntax-class #:for-label Refinement)
(define-literal-syntax-class #:for-label Instance)
(define-literal-syntax-class #:for-label List)
(define-literal-syntax-class #:for-label List*)
(define-literal-syntax-class #:for-label pred)
(define-literal-syntax-class #:for-label ->)
(define-literal-syntax-class #:for-label ->*)
(define-literal-syntax-class #:for-label case->^ (case-> case-lambda))
(define-literal-syntax-class #:for-label Rec)
(define-literal-syntax-class #:for-label U)
(define-literal-syntax-class #:for-label All)
(define-literal-syntax-class #:for-label Opaque)
(define-literal-syntax-class #:for-label Parameter)
(define-literal-syntax-class #:for-label Vector)
(define-literal-syntax-class #:for-label Struct)
(define-literal-syntax-class #:for-label Struct-Type)
(define-literal-syntax-class #:for-label Values)
(define-literal-syntax-class #:for-label values)
(define-literal-syntax-class #:for-label Top)
(define-literal-syntax-class #:for-label Bot)

;; (Syntax -> Type) -> Syntax Any -> Syntax
;; See `parse-type/id`. This is a curried generalization.
(define ((parse/id p) loc datum)
  (let* ([stx* (datum->syntax loc datum loc loc)])
    (p stx*)))

(define (parse-literal-alls stx)
  (syntax-parse stx
    [(:All^ (~or (vars:id ... v:id dd:ddd) (vars:id ...)) . t:omit-parens)
     (define vars-list (syntax->list #'(vars ...)))
     (cons (if (attribute v)
               (list vars-list #'v)
               vars-list)
           (parse-literal-alls #'t.type))]
    [_ null]))


;; Syntax -> Type
;; Parse a Forall type
(define (parse-all-type stx)
  (syntax-parse stx
    [(:All^ (vars:id ... v:id dd:ddd) . t:omit-parens)
     (define maybe-dup (check-duplicate-identifier (syntax->list #'(vars ... v))))
     (when maybe-dup
       (parse-error "duplicate type variable or index"
                    "variable or index" (syntax-e maybe-dup)))
     (let* ([vars (stx-map syntax-e #'(vars ...))]
            [v (syntax-e #'v)])
       (extend-indexes v
         (extend-tvars vars
           (make-PolyDots (append vars (list v)) (parse-type #'t.type)))))]
    [(:All^ (vars:id ...) . t:omit-parens)
     (define maybe-dup (check-duplicate-identifier (syntax->list #'(vars ...))))
     (when maybe-dup
       (parse-error "duplicate type variable"
                    "variable" (syntax-e maybe-dup)))
     (let* ([vars (stx-map syntax-e #'(vars ...))])
       (extend-tvars vars
         (make-Poly vars (parse-type #'t.type))))]
    ;; Next two are row polymorphic cases
    [(:All^ (var:id #:row) . t:omit-parens)
     (add-disappeared-use #'kw)
     (define var* (syntax-e #'var))
     ;; When we're inferring the row constraints, there
     ;; should be no need to extend the constraint environment
     (define body-type
       (extend-tvars (list var*) (parse-type #'t.type)))
     (make-PolyRow
      (list var*)
      ;; No constraints listed, so we need to infer the constraints
      (infer-row-constraints body-type)
      body-type)]
    [(:All^ (var:id #:row constr:row-constraints) . t:omit-parens)
     (add-disappeared-use #'kw)
     (define var* (syntax-e #'var))
     (define constraints (attribute constr.constraints))
     (extend-row-constraints (list var*) (list constraints)
       (extend-tvars (list var*)
         (make-PolyRow
          (list var*)
          constraints
          (parse-type #'t.type))))]
    [(:All^ (_:id ...) _ _ _ ...) (parse-error "too many forms in body of All type")]
    [(:All^ . rest) (parse-error "bad syntax")]))

;; syntax class for standard keyword syntax (same as contracts), may be
;; optional or mandatory depending on where it's used
(define-splicing-syntax-class plain-kw-tys
  (pattern (~seq k:keyword t:expr)
           #:attr mand-kw (delay (make-Keyword (syntax-e #'k) (parse-type #'t) #t))
           #:attr opt-kw  (delay (make-Keyword (syntax-e #'k) (parse-type #'t) #f))))

(define-splicing-syntax-class keyword-tys
  (pattern kw:plain-kw-tys #:attr Keyword (attribute kw.mand-kw))
  ;; custom optional keyword syntax for TR
  (pattern (~seq [k:keyword t:expr])
           #:attr Keyword (delay (make-Keyword (syntax-e #'k) (parse-type #'t) #f))))

(define-syntax-class non-keyword-ty
  (pattern (k:expr e ...))
  (pattern (~and t:expr (~not :colon^) (~not :->^))
           #:when (not (syntax->list #'t))))

;; syntax classes for parsing ->* function types
(define-syntax-class ->*-mand
  #:description "mandatory arguments for ->*"
  #:attributes (doms kws)
  (pattern (dom:non-keyword-ty ... kw:plain-kw-tys ...)
           #:attr doms (syntax->list #'(dom ...))
           #:attr kws  (attribute kw.mand-kw)))

(define-splicing-syntax-class ->*-opt
  #:description "optional arguments for ->*"
  #:attributes (doms kws)
 (pattern (~optional (dom:non-keyword-ty ... kw:plain-kw-tys ...))
          #:attr doms (if (attribute dom)
                          (syntax->list #'(dom ...))
                          null)
          #:attr kws (if (attribute kw)
                         (attribute kw.opt-kw)
                         null)))

(define-splicing-syntax-class ->*-rest
  #:description "rest argument type for ->*"
  #:attributes (type)
  (pattern (~optional (~seq #:rest type:non-keyword-ty))))

;; syntax classes for filters, objects, and related things
(define-syntax-class path-elem
  #:description "path element"
  (pattern :car^
           #:attr pe (make-CarPE))
  (pattern :cdr^
           #:attr pe (make-CdrPE)))


(define-syntax-class @
  #:description "@"
  (pattern (~datum @)))

(define-syntax-class !
  #:description "!"
  (pattern (~datum !)))

(define-splicing-syntax-class simple-latent-filter
  #:description "latent filter"
  (pattern (~seq t:expr :@ pe:path-elem ...)
           #:attr type (parse-type #'t)
           #:attr path (attribute pe.pe))
  (pattern t:expr
           #:attr type (parse-type #'t)
           #:attr path '()))

(define-syntax-class (prop doms)
  #:description "filter proposition"
  #:attributes (prop)
  (pattern :Top^ #:attr prop -top)
  (pattern :Bot^ #:attr prop -bot)
  ;; Here is wrong check
  (pattern (t:expr :@ ~! pe:path-elem ... (~var o (filter-object doms)))
           #:attr prop (-filter (parse-type #'t) (-acc-path (attribute pe.pe) (attribute o.obj))))
  ;; Here is wrong check
  (pattern (:! t:expr :@ ~! pe:path-elem ... (~var o (filter-object doms)))
           #:attr prop (-not-filter (parse-type #'t) (-acc-path (attribute pe.pe) (attribute o.obj))))
  (pattern (:! t:expr)
           #:attr prop (-not-filter (parse-type #'t) 0))
  (pattern (and (~var p (prop doms)) ...)
           #:attr prop (apply -and (attribute p.prop)))
  (pattern (or (~var p (prop doms)) ...)
           #:attr prop (apply -or (attribute p.prop)))
  (pattern ((~literal implies) (~var p1 (prop doms)) (~var p2 (prop doms)))
           #:attr prop (-imp (attribute p1.prop) (attribute p2.prop)))
  (pattern t:expr
           #:attr prop (-filter (parse-type #'t) 0)))

(define-splicing-syntax-class (filter-object doms)
  #:description "filter object"
  #:attributes (obj)
  (pattern i:id
           #:fail-unless (identifier-binding #'i)
           "Filters for predicates may not reference identifiers that are unbound"
           #:fail-when (is-var-mutated? #'i)
           "Filters for predicates may not reference identifiers that are mutated"
           #:attr obj (-id-path #'i))
  (pattern idx:nat
           #:do [(define arg (syntax-e #'idx))]
           #:fail-unless (< arg (length doms))
           (format "Filter proposition's object index ~a is larger than argument length ~a"
                   arg (length doms))
           #:attr obj (-arg-path arg 0))
  (pattern (~seq depth-idx:nat idx:nat)
           #:do [(define arg (syntax-e #'idx))
                 (define depth (syntax-e #'depth-idx))]
           #:fail-unless (<= depth (length (current-arities)))
           (format "Index ~a used in a filter, but the use is only within ~a enclosing functions"
                   depth (length (current-arities)))
           #:do [(define actual-arg
                   (if (zero? depth)
                       (length doms)
                       (list-ref (current-arities) (sub1 depth))))]
           #:fail-unless (< arg actual-arg)
           (format "Filter proposition's object index ~a is larger than argument length ~a"
                   depth actual-arg)
           #:attr obj (-arg-path arg (syntax-e #'depth-idx))))


(define-syntax-class object
  #:attributes (object)
  (pattern e:expr
           #:attr object -empty-obj))

(define-splicing-syntax-class (full-latent doms)
  #:description "latent propositions and object"
  (pattern (~seq (~optional (~seq #:+ (~var p+ (prop doms)) ...+) #:defaults ([(p+.prop 1) null]))
                 (~optional (~seq #:- (~var p- (prop doms)) ...+) #:defaults ([(p-.prop 1) null]))
                 (~optional (~seq #:object o:object)))
           #:attr positive (apply -and (attribute p+.prop))
           #:attr negative (apply -and (attribute p-.prop))
           #:attr object (or (attribute o.object) -empty-obj)))

(define (parse-types stx-list)
  (stx-map parse-type stx-list))

(define (parse-quoted-type stx)
  (syntax-parse stx
    [(t1 . t2)
     (-pair (parse-quoted-type #'t1) (parse-quoted-type #'t2))]
    [t
     (-val (syntax->datum #'t))]))

(define (parse-type stx)
  (parameterize ([current-orig-stx stx])
    (syntax-parse
        stx
      [t
       #:declare t (3d Type/c?)
       (attribute t.datum)]
      [(fst . rst)
       #:fail-unless (not (syntax->list #'rst)) #f
       (-pair (parse-type #'fst) (parse-type #'rst))]
      [(:Class^ e ...)
       (parse-class-type stx)]
      [(:Object^ e ...)
       (parse-object-type stx)]
      [(:Refinement^ p?:id)
       (match (lookup-type/lexical #'p?)
         [(and t (Function: (list (arr: (list dom) _ #f #f '()))))
          (make-Refinement dom #'p?)]
         [t (parse-error "expected a predicate for argument to Refinement"
                         "given" t)])]
      [(:Struct^ t)
       (let ([v (parse-type #'t)])
         (match (resolve v)
           [(and s (? Struct?)) (make-StructTop s)]
           [_ (parse-error #:delayed? #t
                           "expected a structure type for argument to Struct"
                           "given" v)
              (Un)]))]
      [(:Struct-Type^ t)
       (define v (parse-type #'t))
       (match (resolve v)
         [(? Struct? s) (make-StructType s)]
         [_ (parse-error #:delayed? #t
                         "expected a structure type for argument to Struct-Type"
                         "given" v)
            (Un)])]
      [(:Instance^ t)
       (let ([v (parse-type #'t)])
         (if (not (or (F? v) (Mu? v) (Name? v) (Class? v) (Error? v)))
             (begin (parse-error #:delayed? #t
                                 "expected a class type for argument to Instance"
                                 "given" v)
                    (make-Instance (Un)))
             (make-Instance v)))]
      [(:List^ ts ...)
       (parse-list-type stx)]
      [(:List*^ ts ... t)
       (-Tuple* (parse-types #'(ts ...)) (parse-type #'t))]
      [(:Vector^ ts ...)
       (make-HeterogeneousVector (parse-types #'(ts ...)))]
      [(:cons^ fst rst)
       (-pair (parse-type #'fst) (parse-type #'rst))]
      [(:pred^ t)
       (make-pred-ty (parse-type #'t))]
      [(:case->^ tys ...)
       (make-Function
        (for/list ([ty (in-syntax #'(tys ...))])
          (let ([t (parse-type ty)])
            (match t
              [(Function: (list arr)) arr]
              [_ (parse-error
                  #:stx ty
                  "expected a function type for component of case-> type"
                  "given" t)]))))]
      [(:Rec^ x:id t)
       (let* ([var (syntax-e #'x)]
              [tvar (make-F var)])
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
                  [(PolyRow: _ _ body) (loop body)]
                  [else #t])))
             (unless productive
               (parse-error
                #:stx stx
                "recursive types are not allowed directly inside their definition"))
             (if (memq var (fv t*))
                 (make-Mu var t*)
                 t*))))]
      [(:U^ ts ...)
       (apply Un (parse-types #'(ts ...)))]
      [(:quote^ t)
       (parse-quoted-type #'t)]
      [(:All^ . rest)
       (parse-all-type stx)]
      [(:Opaque^ p?:id)
       (make-Opaque #'p?)]
      [(:Parameter^ t)
       (let ([ty (parse-type #'t)])
         (-Param ty ty))]
      [(:Parameter^ t1 t2)
       (-Param (parse-type #'t1) (parse-type #'t2))]
      ;; curried function notation
      [((~and dom:non-keyword-ty (~not :->^)) ...
        :->^
        (~and (~seq rest-dom ...) (~seq (~or _ (~between :->^ 1 +inf.0)) ...)))
       (define doms (syntax->list #'(dom ...)))
       (with-arity (length doms)
         (let ([doms (for/list ([d (in-list doms)])
                       (parse-type d))])
           (make-Function
            (list (make-arr
                   doms
                   (parse-type (syntax/loc stx (rest-dom ...))))))))]
      [(~or (:->^ dom rng :colon^ latent:simple-latent-filter)
            (dom :->^ rng :colon^ latent:simple-latent-filter))
       ;; use parse-type instead of parse-values-type because we need to add the filters from the pred-ty
       (with-arity 1
         (make-pred-ty (list (parse-type #'dom)) (parse-type #'rng) (attribute latent.type)
                       (-acc-path (attribute latent.path) (-arg-path 0))))]
      [(~or (:->^ dom:non-keyword-ty ... kws:keyword-tys ... rest:non-keyword-ty ddd:star rng)
            (dom:non-keyword-ty ... kws:keyword-tys ... rest:non-keyword-ty ddd:star :->^ rng))
       (with-arity (length (syntax->list #'(dom ...)))
         (make-Function
          (list (make-arr
                 (parse-types #'(dom ...))
                 (parse-values-type #'rng)
                 #:rest (parse-type #'rest)
                 #:kws (map force (attribute kws.Keyword))))))]
      [(~or (:->^ dom:non-keyword-ty ... rest:non-keyword-ty :ddd/bound rng)
            (dom:non-keyword-ty ... rest:non-keyword-ty :ddd/bound :->^ rng))
       (with-arity (length (syntax->list #'(dom ...)))
         (let* ([bnd (syntax-e #'bound)])
           (unless (bound-index? bnd)
             (parse-error
              #:stx #'bound
              "used a type variable not bound with ... as a bound on a ..."
              "variable" bnd))
           (make-Function
            (list
             (make-arr-dots (parse-types #'(dom ...))
                            (parse-values-type #'rng)
                            (extend-tvars (list bnd)
                                          (parse-type #'rest))
                            bnd)))))]
      [(~or (:->^ dom:non-keyword-ty ... rest:non-keyword-ty _:ddd rng)
            (dom:non-keyword-ty ... rest:non-keyword-ty _:ddd :->^ rng))
       (with-arity (length (syntax->list #'(dom ...)))
         (let ([var (infer-index stx)])
           (make-Function
            (list
             (make-arr-dots (parse-types #'(dom ...))
                            (parse-values-type #'rng)
                            (extend-tvars (list var) (parse-type #'rest))
                            var)))))]
      #| ;; has to be below the previous one
     [(dom:expr ... :->^ rng)
      (->* (parse-types #'(dom ...))
           (parse-values-type #'rng))]     |#
      ;; use expr to rule out keywords
      [(~or (:->^ dom:non-keyword-ty ... kws:keyword-tys ... rng)
            (dom:non-keyword-ty ... kws:keyword-tys ... :->^ rng))
       (define doms (syntax->list #'(dom ...)))
       (with-arity (length doms)
         (let ([doms (for/list ([d (in-list doms)])
                       (parse-type d))])
           (make-Function
            (list (make-arr
                   doms
                   (parse-values-type #'rng)
                   #:kws (map force (attribute kws.Keyword)))))))]
      ;; This case needs to be at the end because it uses cut points to give good error messages.
      [(~or (:->^ ~! dom:non-keyword-ty ... rng:expr
             :colon^ (~var latent (full-latent (syntax->list #'(dom ...)))))
            (dom:non-keyword-ty ... :->^ rng:expr
             ~! :colon^ (~var latent (full-latent (syntax->list #'(dom ...))))))
       ;; use parse-type instead of parse-values-type because we need to add the filters from the pred-ty
       (with-arity (length (syntax->list #'(dom ...)))
         (->* (parse-types #'(dom ...))
              (parse-type #'rng)
              : (-FS (attribute latent.positive) (attribute latent.negative))
              : (attribute latent.object)))]
      [(:->*^ mand:->*-mand opt:->*-opt rest:->*-rest rng)
       (with-arity (length (attribute mand.doms))
         (define doms (for/list ([d (attribute mand.doms)])
                        (parse-type d)))
         (define opt-doms (for/list ([d (attribute opt.doms)])
                            (parse-type d)))
         (opt-fn doms opt-doms (parse-values-type #'rng)
                 #:rest (and (attribute rest.type)
                             (parse-type (attribute rest.type)))
                 #:kws (map force (append (attribute mand.kws)
                                          (attribute opt.kws)))))]
      [:->^
       (parse-error #:delayed? #t "incorrect use of -> type constructor")
       Err]
      [id:identifier
       (cond
         ;; if it's a type variable, we just produce the corresponding reference (which is in the HT)
         [(bound-tvar? (syntax-e #'id))
          (lookup-tvar (syntax-e #'id))]
         ;; if it was in current-indexes, produce a better error msg
         [(bound-index? (syntax-e #'id))
          (parse-error "type variable must be used with ..."
                       "variable" (syntax-e #'id))]
         ;; if it's a type alias, we expand it (the expanded type is stored in the HT)
         [(lookup-type-alias #'id parse-type (lambda () #f))
          =>
          (lambda (t)
            (when (current-referenced-aliases)
              (define alias-box (current-referenced-aliases))
              (set-box! alias-box (cons #'id (unbox alias-box))))
            (add-disappeared-use (syntax-local-introduce #'id))
            t)]
         [else
          (parse-error #:delayed? #t (~a "type name `" (syntax-e #'id) "' is unbound"))
          Err])]
      [(:Opaque^ . rest)
       (parse-error "bad syntax in Opaque")]
      [(:U^ . rest)
       (parse-error "bad syntax in Union")]
      [(:Rec^ . rest)
       (parse-error "bad syntax in Rec")]
      [(t ... :->^ . rest)
       (parse-error "bad syntax in ->")]
      [(id arg args ...)
       (let loop
         ([rator (parse-type #'id)]
          [args (parse-types #'(arg args ...))])
         (resolve-app-check-error rator args stx)
         (match rator
           [(Name: _ _ _ _) (make-App rator args stx)]
           [(Poly: _ _) (instantiate-poly rator args)]
           [(Mu: _ _) (loop (unfold rator) args)]
           [(Error:) Err]
           [_ Err]))]
      [t:atom
       ;; Integers in a "grey area", that is, integers whose runtime type is
       ;; platform-dependent, cannot be safely assigned singleton types.
       ;; Short story: (subtype (-val 10000000000000) -Fixnum) has no safe
       ;;   answer. It's not a fixnum on 32 bits, and saying it's not a fixnum
       ;;   causes issues with type-dead code detection.
       ;; Long story: See email trail for PR13501 and #racket IRC logs from
       ;;   Feb 11 2013.
       (let ([val (syntax-e #'t)])
         (when (and (exact-integer? val)
                    ;; [min-64bit-fixnum, min-portable-fixnum)
                    (or (and (>= val (- (expt 2 62)))
                             (<  val (- (expt 2 30))))
                        ;; (max-portable-index, max-64bit-fixnum]
                        (and (>  val (sub1 (expt 2 28)))
                             (<= val (sub1 (expt 2 62))))))
           (parse-error "non-portable fixnum singleton types are not valid types"
                        "given" val))
         (-val val))]
      [_ (parse-error "expected a valid type"
                      "given" (syntax->datum stx))])))

;; Syntax -> Type
;; Parse a (List ...) type
(define (parse-list-type stx)
  (parameterize ([current-orig-stx stx])
    (syntax-parse stx
      [(:List^ tys ... dty :ddd/bound)
       (let ([var (syntax-e #'bound)])
         (unless (bound-index? var)
           (if (bound-tvar? var)
               (tc-error/stx #'bound "Used a type variable (~a) not bound with ... as a bound on a ..." var)
               (tc-error/stx #'bound "Type variable ~a is unbound" var)))
         (-Tuple* (parse-types #'(tys ...))
                  (make-ListDots
                   (extend-tvars (list var)
                     (parse-type #'dty))
                   var)))]
      [(:List^ tys ... dty _:ddd)
       (let ([var (infer-index stx)])
         (-Tuple* (parse-types #'(tys ...))
                  (make-ListDots
                    (extend-tvars (list var)
                      (parse-type #'dty))
                    var)))]
      [(:List^ tys ...)
       (-Tuple (parse-types #'(tys ...)))])))

;; Syntax -> Type
;; Parse a (Values ...) type
(define (parse-values-type stx)
  (parameterize ([current-orig-stx stx])
    (syntax-parse stx
      [((~or :Values^ :values^) tys ... dty :ddd/bound)
       (let ([var (syntax-e #'bound)])
         (unless (bound-index? var)
           (if (bound-tvar? var)
               (tc-error/stx #'bound "Used a type variable (~a) not bound with ... as a bound on a ..." var)
               (tc-error/stx #'bound "Type variable ~a is unbound" var)))
         (-values-dots (parse-types #'(tys ...))
                       (extend-tvars (list var)
                         (parse-type #'dty))
                       var))]
      [((~or :Values^ :values^) tys ... dty _:ddd)
       (let ([var (infer-index stx)])
         (-values-dots (parse-types #'(tys ...))
                       (extend-tvars (list var)
                         (parse-type #'dty))
                       var))]
      [((~or :Values^ :values^) tys ...)
       (-values (parse-types #'(tys ...)))]
      [t
       (-values (list (parse-type #'t)))])))

;;; Utilities for (Class ...) type parsing

;; process-class-clauses : Option<F> Type Stx FieldDict MethodDict AugmentDict
;;                         -> Option<Id> FieldDict MethodDict AugmentDict
;; Merges #:implements class type and the current class clauses appropriately
(define (merge-with-parent-type row-var parent-type parent-stx fields methods augments)
  ;; (Listof Symbol) Dict Dict String -> (Values Dict Dict)
  ;; check for duplicates in a class clause
  (define (check-duplicate-clause names super-names types super-types err-msg)
    (define maybe-dup (check-duplicate (append names super-names)))
    (cond [maybe-dup
           (define type (car (dict-ref types maybe-dup)))
           (define super-type (car (dict-ref super-types maybe-dup)))
           (cond [;; if there is a duplicate, but the type is a subtype,
                  ;; then let it through and check for any other duplicates
                  (unless (subtype type super-type)
                    (parse-error "class member type not a subtype of parent member type"
                                 "member" maybe-dup
                                 "type" type
                                 "parent type" super-type))
                  (check-duplicate-clause
                   names (remove maybe-dup super-names)
                   types (dict-remove super-types maybe-dup)
                   err-msg)]
                 [else
                  (parse-error #:stx parent-stx err-msg "name" maybe-dup)])]
          [else (values types super-types)]))

  (define (match-parent-type parent-type)
    (define resolved (resolve parent-type))
    (match resolved
      [(Class: row-var _ fields methods augments _)
       (values row-var fields methods augments)]
      [_ (parse-error "expected a class type for #:implements clause"
                      "given" resolved)]))
  (define-values (super-row-var super-fields
                  super-methods super-augments)
    (match-parent-type parent-type))

  (match-define (list (list field-names _) ...) fields)
  (match-define (list (list method-names _) ...) methods)
  (match-define (list (list augment-names _) ...) augments)
  (match-define (list (list super-field-names _) ...) super-fields)
  (match-define (list (list super-method-names _) ...) super-methods)
  (match-define (list (list super-augment-names _) ...) super-augments)

  ;; if any duplicates are found between this class and the superclass
  ;; type, then raise an error
  (define-values (checked-fields checked-super-fields)
    (check-duplicate-clause
     field-names super-field-names
     fields super-fields
     "field or init-field name conflicts with #:implements clause"))
  (define-values (checked-methods checked-super-methods)
    (check-duplicate-clause
     method-names super-method-names
     methods super-methods
     "method name conflicts with #:implements clause"))
  (define-values (checked-augments checked-super-augments)
    (check-duplicate-clause
     augment-names super-augment-names
     augments super-augments
     "augmentable method name conflicts with #:implements clause"))

  ;; it is an error for both the extending type and extended type
  ;; to have row variables
  (when (and row-var super-row-var)
    (parse-error (~a "class type with row variable cannot"
                     " extend another type that has a row variable")))

  ;; then append the super types if there were no errors
  (define merged-fields (append checked-super-fields checked-fields))
  (define merged-methods (append checked-super-methods checked-methods))
  (define merged-augments (append checked-super-augments checked-augments))

  ;; make sure augments and methods are disjoint
  (define maybe-dup-method (check-duplicate (dict-keys merged-methods)))
  (when maybe-dup-method
    (parse-error "duplicate method name" "name" maybe-dup-method))
  (define maybe-dup-augment (check-duplicate (dict-keys merged-augments)))
  (when maybe-dup-augment
    (parse-error "duplicate augmentable method name"
                 "name" maybe-dup-augment))

  (values (or row-var super-row-var) merged-fields
          merged-methods merged-augments))

;; Syntax -> Type
;; Parse a (Object ...) type
;; This is an alternative way to write down an Instance type
(define (parse-object-type stx)
  (syntax-parse stx
    [(kw clause:object-type-clauses)
     (add-disappeared-use #'kw)
     (define fields (map list
                         (stx-map syntax-e #'clause.field-names)
                         (stx-map parse-type #'clause.field-types)))
     (define methods (map list
                          (stx-map syntax-e #'clause.method-names)
                          (stx-map parse-type #'clause.method-types)))
     (check-function-types methods)
     (make-Instance (make-Class #f null fields methods null #f))]))

;; Syntax -> Type
;; Parse a (Class ...) type
(define (parse-class-type stx)
  (syntax-parse stx
    [(kw (~var clause (class-type-clauses parse-type)))
     (add-disappeared-use #'kw)
     (define parent-stxs (stx->list #'clause.extends-types))
     (define parent-types (map parse-type parent-stxs))
     (define given-inits (attribute clause.inits))
     (define given-fields (attribute clause.fields))
     (define given-methods (attribute clause.methods))
     (define given-augments (attribute clause.augments))
     (define given-row-var
       (and (attribute clause.row-var)
            (parse-type (attribute clause.row-var))))
     (define given-init-rest
       (and (attribute clause.init-rest)
            (parse-type (attribute clause.init-rest))))

     (cond ;; If an Error type flows into the #:row-var position, a
           ;; delayed error should be raised from the recursive call to
           ;; `parse-type` so no additional error is needed here.
           [(Error? given-row-var) Err]
           [(and given-row-var (not (F? given-row-var)))
            (parse-error "expected a type variable for #:row-var"
                         "given" given-row-var)
            Err]
           ;; Only proceed to create a class type when the parsing
           ;; process isn't looking for recursive type alias references.
           ;; (otherwise the merging process will error)
           [(or (null? parent-stxs)
                (not (current-referenced-aliases)))

            (check-function-types given-methods)
            (check-function-types given-augments)

            ;; merge with all given parent types, erroring if needed
            (define-values (row-var fields methods augments)
              (for/fold ([row-var given-row-var]
                         [fields given-fields]
                         [methods given-methods]
                         [augments given-augments])
                  ([parent-type parent-types]
                   [parent-stx  parent-stxs])
                (merge-with-parent-type row-var parent-type parent-stx
                                        fields methods augments)))

            ;; check constraints on row var for consistency with class
            (when (and row-var (has-row-constraints? (F-n row-var)))
              (define constraints (lookup-row-constraints (F-n row-var)))
              (check-constraints given-inits (car constraints))
              (check-constraints fields (cadr constraints))
              (check-constraints methods (caddr constraints))
              (check-constraints augments (cadddr constraints)))

            (define class-type
              (make-Class row-var given-inits fields methods augments given-init-rest))

            class-type]
           [else
            ;; Conservatively assume that if there *are* #:implements
            ;; clauses, then the current type alias will be recursive
            ;; through one of the type aliases in the #:implements clauses.
            ;;
            ;; This is needed because it's hard to determine if a type
            ;; in the #:implements clauses depends on the current
            ;; type alias at this point. Otherwise, we would have to
            ;; parse all type aliases again.
            ;;
            ;; An example type that is a problem without this assumption is
            ;;   alias = (Class #:implements Foo%) where Foo%
            ;;           has a class clause referring to alias
            ;; since "alias" will be a non-recursive alias
            ;;
            ;; Without the approximation, we may miss recursive references
            ;; which can cause infinite looping elsewhere in TR.
            ;;
            ;; With the approximation, we have spurious recursive references
            ;; which may cause more indirection through the Name environment
            ;; or generate worse contracts.
            (define alias-box (current-referenced-aliases))
            (set-box! alias-box (cons (current-type-alias-name)
                                      (unbox alias-box)))
            (define class-box (current-referenced-class-parents))
            (set-box! class-box (append parent-stxs (unbox class-box)))
            ;; Ok to return Error here, since this type will
            ;; get reparsed in another pass
            (make-Error)
            ])]))

;; check-function-types : Dict<Name, Type> -> Void
;; ensure all types recorded in the dictionary are function types
(define (check-function-types method-types)
  ;; TODO: this function should probably go in a utility
  ;;       module since it's duplicated elsewhere
  (define (function-type? type)
    (match (resolve type)
      [(? Function?) #t]
      [(Poly: _ body) (function-type? body)]
      [(PolyDots: _ body) (function-type? body)]
      [(PolyRow: _ _ body) (function-type? body)]
      [_ #f]))
  (for ([(id pre-type) (in-dict method-types)])
    (define type (car pre-type))
    (unless (function-type? type)
      (parse-error "method must have a function type"
                   "method name" id
                   "given type" type))))

;; check-constraints : Dict<Name, _> Listof<Name> -> Void
;; helper to check if the constraints are consistent with the type
(define (check-constraints type-table constraint-names)
  (define names-from-type (dict-keys type-table))
  (define conflicting-name
    (for/or ([m (in-list names-from-type)])
      (and (not (memq m constraint-names))
           m)))
  (when conflicting-name
    (parse-error "class member conflicts with row variable constraints"
                 "conflicting name" conflicting-name)))

(define (parse-tc-results stx)
  (syntax-parse stx
    [(:values^ t ...)
     (ret (parse-types #'(t ...))
          (stx-map (lambda (x) -no-filter) #'(t ...))
          (stx-map (lambda (x) -no-obj) #'(t ...)))]
    [t (ret (parse-type #'t) -no-filter -no-obj)]))

(define parse-type/id (parse/id parse-type))

;; parse-error : String String String ... ... -> Void
;; helper for parse-type error messages
(define (parse-error reason
                     #:delayed? [delayed? #f]
                     #:stx [stx (current-orig-stx)]
                     . rst)
  (apply tc-error/fields "parse error in type"
                         #:more reason
                         #:delayed? delayed?
                         rst))
