#lang racket/base

;; This module provides functions for parsing types written by the user

(require "../utils/utils.rkt"
         (except-in (rep type-rep object-rep filter-rep) make-arr)
         (rename-in (types abbrev union utils filter-ops resolve classes)
                    [make-arr* make-arr])
         (utils tc-utils stxclass-util literal-syntax-class)
         syntax/stx (prefix-in c: (contract-req))
         syntax/parse unstable/sequence
         (env tvar-env type-name-env type-alias-env
              lexical-env index-env row-constraint-env)
         (only-in racket/list flatten)
         racket/dict
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
                       [parse-tc-results/id (syntax? c:any/c . c:-> . tc-results/c)]
                       [parse-literal-alls (syntax? . c:-> . (c:listof (c:or/c (c:listof identifier?) (c:list/c (c:listof identifier?) identifier?))))])

(provide star ddd/bound)

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
  #;(printf "parse-type/id id : ~a\n ty: ~a\n" (syntax-object->datum loc) (syntax-object->datum stx))
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
  ;(printf "parse-all-type: ~a \n" (syntax->datum stx))
  (syntax-parse stx
    [(:All^ (vars:id ... v:id dd:ddd) . t:omit-parens)
     (when (check-duplicate-identifier (syntax->list #'(vars ... v)))
       (tc-error "All: duplicate type variable or index"))
     (let* ([vars (stx-map syntax-e #'(vars ...))]
            [v (syntax-e #'v)])
       (extend-indexes v
         (extend-tvars vars
           (make-PolyDots (append vars (list v)) (parse-type #'t.type)))))]
    [(:All^ (vars:id ...) . t:omit-parens)
     (when (check-duplicate-identifier (syntax->list #'(vars ...)))
       (tc-error "All: duplicate type variable"))
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
    [(:All^ (_:id ...) _ _ _ ...) (tc-error "All: too many forms in body of All type")]
    [(:All^ . rest) (tc-error "All: bad syntax")]))

;; syntax class for standard keyword syntax (same as contracts), may be
;; optional or mandatory depending on where it's used
(define-splicing-syntax-class plain-kw-tys
  (pattern (~seq k:keyword t:expr)
           #:attr mand-kw (make-Keyword (syntax-e #'k) (parse-type #'t) #t)
           #:attr opt-kw  (make-Keyword (syntax-e #'k) (parse-type #'t) #f)))

(define-splicing-syntax-class keyword-tys
  (pattern kw:plain-kw-tys #:attr Keyword (attribute kw.mand-kw))
  ;; custom optional keyword syntax for TR
  (pattern (~seq [k:keyword t:expr])
           #:attr Keyword (make-Keyword (syntax-e #'k) (parse-type #'t) #f)))

(define-syntax-class non-keyword-ty
  (pattern (k e ...)
           #:when (not (keyword? (syntax->datum #'k))))
  (pattern t:expr
           #:when (and (not (keyword? (syntax->datum #'t)))
                       (not (syntax->list #'t)))))

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

(define-splicing-syntax-class idx-obj
  #:description "index object"
  #:attributes (arg depth pair)
  (pattern (~seq idx:nat)
           #:attr arg (syntax-e #'idx)
           #:attr depth 0
           #:attr pair (list 0 (syntax-e #'idx)))
  (pattern (~seq depth-idx:nat idx:nat)
           #:attr arg (syntax-e #'idx)
           #:attr depth (syntax-e #'depth-idx)
           #:attr pair (list (syntax-e #'depth-idx)
                             (syntax-e #'idx))))

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
  (pattern (t:expr :@ pe:path-elem ... i:idx-obj)
           #:fail-unless (< (attribute i.arg) (length doms))
           (format "Filter proposition's object index ~a is larger than argument length ~a"
                   (attribute i.arg) (length doms))
           #:attr prop (-filter (parse-type #'t) (attribute i.pair) (attribute pe.pe)))
  (pattern (t:expr :@ pe:path-elem ... i:id)
           #:attr prop (-filter (parse-type #'t) #'i (attribute pe.pe)))
  (pattern (:! t:expr :@ pe:path-elem ... i:idx-obj)
           #:fail-unless (< (attribute i.arg) (length doms))
           (format "Filter proposition's object index ~a is larger than argument length ~a"
                   (attribute i.arg) (length doms))
           #:attr prop (-not-filter (parse-type #'t) (attribute i.pair) (attribute pe.pe)))
  (pattern (:! t:expr :@ pe:path-elem ... i:id)
           #:attr prop (-not-filter (parse-type #'t) #'i (attribute pe.pe)))
  (pattern (and (~var p (prop doms)) ...)
           #:attr prop (apply -and (attribute p.prop)))
  (pattern (or (~var p (prop doms)) ...)
           #:attr prop (apply -or (attribute p.prop)))
  (pattern ((~literal implies) (~var p1 (prop doms)) (~var p2 (prop doms)))
           #:attr prop (-imp (attribute p1.prop) (attribute p2.prop))))

(define-syntax-class object
  #:attributes (object)
  (pattern e:expr
           #:attr object -no-obj))

(define-splicing-syntax-class (full-latent doms)
  #:description "latent propositions and object"
  (pattern (~seq (~optional (~seq #:+ (~var p+ (prop doms)) ...+) #:defaults ([(p+.prop 1) null]))
                 (~optional (~seq #:- (~var p- (prop doms)) ...+) #:defaults ([(p-.prop 1) null]))
                 (~optional (~seq #:object o:object)))
           #:attr positive (apply -and (attribute p+.prop))
           #:attr negative (apply -and (attribute p-.prop))
           #:attr object (or (attribute o.object) -no-obj)))

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
         [t (tc-error "cannot declare refinement for non-predicate ~a" t)])]
      [(:Struct^ t)
       (let ([v (parse-type #'t)])
         (match (resolve v)
           [(and s (? Struct?)) (make-StructTop s)]
           [_ (tc-error/delayed "Argument to Struct must be a structure type, got ~a" v)
              (Un)]))]
      [(:Struct-Type^ t)
       (define v (parse-type #'t))
       (match (resolve v)
         [(? Struct? s) (make-StructType s)]
         [_ (tc-error/delayed "Argument to Struct-Type must be a structure type, got ~a" v)
            (Un)])]
      [(:Instance^ t)
       (let ([v (parse-type #'t)])
         (if (not (or (F? v) (Mu? v) (Name? v) (Class? v) (Error? v)))
             (begin (tc-error/delayed "Argument to Instance must be a class type, got ~a" v)
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
              [_ (tc-error/stx
                  ty
                  "Component of case-lambda type was not a function clause")]))))]
      #;[(:Vectorof^ t)
       (make-Vector (parse-type #'t))]
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
                  [else #t])))
             (unless productive
               (tc-error/stx
                stx
                "Recursive types are not allowed directly inside their definition"))
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
       (let ([doms (for/list ([d (in-syntax #'(dom ...))])
                     (parse-type d))])
         (make-Function
          (list (make-arr
                 doms
                 (parse-type (syntax/loc stx (rest-dom ...)))))))]
      [(~or (:->^ dom rng :colon^ latent:simple-latent-filter)
            (dom :->^ rng :colon^ latent:simple-latent-filter))
       ;; use parse-type instead of parse-values-type because we need to add the filters from the pred-ty
       (make-pred-ty (list (parse-type #'dom)) (parse-type #'rng) (attribute latent.type) 0 (attribute latent.path))]
      [(~or (:->^ dom ... rng
             :colon^ ~! (~var latent (full-latent (syntax->list #'(dom ...)))))
            (dom ... :->^ rng
             :colon^ ~! (~var latent (full-latent (syntax->list #'(dom ...))))))
       ;; use parse-type instead of parse-values-type because we need to add the filters from the pred-ty
       (->* (parse-types #'(dom ...))
            (parse-type #'rng)
            : (-FS (attribute latent.positive) (attribute latent.negative))
            : (attribute latent.object))]
      [(~or (:->^ dom:non-keyword-ty ... kws:keyword-tys ... rest:non-keyword-ty ddd:star rng)
            (dom:non-keyword-ty ... kws:keyword-tys ... rest:non-keyword-ty ddd:star :->^ rng))
       (make-Function
        (list (make-arr
               (parse-types #'(dom ...))
               (parse-values-type #'rng)
               #:rest (parse-type #'rest)
               #:kws (attribute kws.Keyword))))]
      [(~or (:->^ dom:non-keyword-ty ... rest:non-keyword-ty :ddd/bound rng)
            (dom:non-keyword-ty ... rest:non-keyword-ty :ddd/bound :->^ rng))
       (let* ([bnd (syntax-e #'bound)])
         (unless (bound-index? bnd)
           (tc-error/stx #'bound
                         "Used a type variable (~a) not bound with ... as a bound on a ..."
                         bnd))
         (make-Function
          (list
           (make-arr-dots (parse-types #'(dom ...))
                          (parse-values-type #'rng)
                          (extend-tvars (list bnd)
                            (parse-type #'rest))
                          bnd))))]
      [(~or (:->^ dom:non-keyword-ty ... rest:non-keyword-ty _:ddd rng)
            (dom:non-keyword-ty ... rest:non-keyword-ty _:ddd :->^ rng))
       (let ([var (infer-index stx)])
         (make-Function
          (list
           (make-arr-dots (parse-types #'(dom ...))
                          (parse-values-type #'rng)
                          (extend-tvars (list var) (parse-type #'rest))
                          var))))]
      #| ;; has to be below the previous one
     [(dom:expr ... :->^ rng)
      (->* (parse-types #'(dom ...))
           (parse-values-type #'rng))]     |#
      ;; use expr to rule out keywords
      [(~or (:->^ dom:non-keyword-ty ... kws:keyword-tys ... rng)
            (dom:non-keyword-ty ... kws:keyword-tys ... :->^ rng))
      (let ([doms (for/list ([d (in-syntax #'(dom ...))])
                    (parse-type d))])
         (make-Function
          (list (make-arr
                 doms
                 (parse-values-type #'rng)
                 #:kws (attribute kws.Keyword)))))]
      [(:->*^ mand:->*-mand opt:->*-opt rest:->*-rest rng)
       (define doms (for/list ([d (attribute mand.doms)])
                      (parse-type d)))
       (define opt-doms (for/list ([d (attribute opt.doms)])
                          (parse-type d)))
       (opt-fn doms opt-doms (parse-values-type #'rng)
               #:rest (and (attribute rest.type)
                           (parse-type (attribute rest.type)))
               #:kws (append (attribute mand.kws)
                             (attribute opt.kws)))]
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
            (add-disappeared-use (syntax-local-introduce #'id))
            t)]
         ;; if it's a type name, we just use the name
         [(lookup-type-name #'id (lambda () #f))
          (add-disappeared-use (syntax-local-introduce #'id))
          ;(printf "found a type name ~a\n" #'id)
          (make-Name #'id)]
         [(free-identifier=? #'id #'->)
          (tc-error/delayed "Incorrect use of -> type constructor")
          Err]
         [else
          (tc-error/delayed
           "Unbound type name ~a"
           (syntax-e #'id))
          Err])]
      [(:Opaque^ . rest)
       (tc-error "Opaque: bad syntax")]
      [(:U^ . rest)
       (tc-error "Union: bad syntax")]
      #;[(:Vectorof^ . rest)
       (tc-error "Vectorof: bad syntax")]
      [(:Rec^ . rest)
       (tc-error "Rec: bad syntax")]
      [(t ... :->^ . rest)
       (tc-error "->: bad syntax")]
      [(id arg args ...)
       (let loop
         ([rator (parse-type #'id)]
          [args (parse-types #'(arg args ...))])
         (resolve-app-check-error rator args stx)
         (match rator
           [(Name: _) (make-App rator args stx)]
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
           (tc-error "non-portable fixnum singleton types are not valid types: ~a" val))
         (-val val))]
      [_ (tc-error "not a valid type: ~a" (syntax->datum stx))])))

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

;; process-class-clauses : Option<F> Syntax FieldDict MethodDict AugmentDict
;;                         -> Option<Id> FieldDict MethodDict AugmentDict
;; Merges #:implements class type and the current class clauses appropriately
(define (merge-with-parent-type row-var stx fields methods augments)
  ;; (Listof Symbol) Dict Dict String -> (Values Dict Dict)
  ;; check for duplicates in a class clause
  (define (check-duplicate-clause names super-names types super-types err-msg)
    (define maybe-dup (check-duplicate (append names super-names)))
    (cond [maybe-dup
           (define type (car (dict-ref types maybe-dup)))
           (define super-type (car (dict-ref super-types maybe-dup)))
           (cond [;; if there is a duplicate, but the type is the same,
                  ;; then let it through and check for any other duplicates
                  (type-equal? type super-type)
                  (check-duplicate-clause
                   (remove maybe-dup names) super-names
                   (dict-remove types maybe-dup) super-types
                   err-msg)]
                 [else
                  (tc-error/stx stx err-msg maybe-dup)])]
          [else (values types super-types)]))

  (define parent-type (parse-type stx))
  (define (match-parent-type parent-type)
    (match parent-type
      [(Class: row-var _ fields methods augments)
       (values row-var fields methods augments)]
      [(? Mu?)
       (match-parent-type (unfold parent-type))]
      [_ (tc-error "expected a class type for #:implements clause, got ~a"
                   parent-type)]))
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
     "field or init-field name ~a conflicts with #:implements clause"))
  (define-values (checked-methods checked-super-methods)
    (check-duplicate-clause
     method-names super-method-names
     methods super-methods
     "method name ~a conflicts with #:implements clause"))
  (define-values (checked-augments checked-super-augments)
    (check-duplicate-clause
     augment-names super-augment-names
     augments super-augments
     "augmentable method name ~a conflicts with #:implements clause"))

  ;; it is an error for both the extending type and extended type
  ;; to have row variables
  (when (and row-var super-row-var)
    (tc-error (~a "class type with row variable cannot"
                  " extend another type that has a row variable")))

  ;; then append the super types if there were no errors
  (define merged-fields (append checked-super-fields checked-fields))
  (define merged-methods (append checked-super-methods checked-methods))
  (define merged-augments (append checked-super-augments checked-augments))

  ;; make sure augments and methods are disjoint
  (define maybe-dup-method (check-duplicate (dict-keys merged-methods)))
  (when maybe-dup-method
    (tc-error (~a "method name " maybe-dup-method " conflicts with"
                  " another method name")))
  (define maybe-dup-augment (check-duplicate (dict-keys merged-augments)))
  (when maybe-dup-augment
    (tc-error (~a "augmentable method name " maybe-dup-augment " conflicts with"
                  " another augmentable method name")))

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
     (make-Instance (make-Class #f null fields methods null))]))

;; Syntax -> Type
;; Parse a (Class ...) type
(define (parse-class-type stx)
  (syntax-parse stx
    [(kw (~var clause (class-type-clauses parse-type)))
     (add-disappeared-use #'kw)

     (define parent-types (stx->list #'clause.extends-types))
     (define given-inits (attribute clause.inits))
     (define given-fields (attribute clause.fields))
     (define given-methods (attribute clause.methods))
     (define given-augments (attribute clause.augments))
     (define given-row-var
       (and (attribute clause.row-var)
            (parse-type (attribute clause.row-var))))

     (check-function-types given-methods)
     (check-function-types given-augments)

     ;; merge with all given parent types, erroring if needed
     (define-values (row-var fields methods augments)
      (for/fold ([row-var given-row-var]
                 [fields given-fields]
                 [methods given-methods]
                 [augments given-augments])
                ([parent-type parent-types])
        (merge-with-parent-type row-var parent-type fields
                                methods augments)))

     ;; check constraints on row var for consistency with class
     (when (and row-var (has-row-constraints? (F-n row-var)))
       (define constraints (lookup-row-constraints (F-n row-var)))
       (check-constraints given-inits (car constraints))
       (check-constraints fields (cadr constraints))
       (check-constraints methods (caddr constraints))
       (check-constraints augments (cadddr constraints)))

     (define class-type
       (make-Class row-var given-inits fields methods augments))

     class-type]))

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
      (tc-error "method ~a must have a function type, given ~a"
                id type))))

;; check-constraints : Dict<Name, _> Listof<Name> -> Void
;; helper to check if the constraints are consistent with the type
(define (check-constraints type-table constraint-names)
  (define names-from-type (dict-keys type-table))
  (define conflicting-name
    (for/or ([m (in-list names-from-type)])
      (and (not (memq m constraint-names))
           m)))
  (when conflicting-name
    (tc-error (~a "class type cannot contain member "
                  conflicting-name
                  " because it conflicts with the row variable constraints"))))

(define (parse-tc-results stx)
  (syntax-parse stx
    [(:values^ t ...)
     (ret (parse-types #'(t ...))
          (stx-map (lambda (x) (make-NoFilter)) #'(t ...))
          (stx-map (lambda (x) (make-NoObject)) #'(t ...)))]
    [t (ret (parse-type #'t) (make-NoFilter) (make-NoObject))]))

(define parse-tc-results/id (parse/id parse-tc-results))

(define parse-type/id (parse/id parse-type))
