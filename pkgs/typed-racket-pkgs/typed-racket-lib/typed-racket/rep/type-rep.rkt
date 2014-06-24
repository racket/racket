#lang racket/base

;; This module provides type representations and utility functions
;; and pattern matchers on types

(require "../utils/utils.rkt")

;; TODO use contract-req
(require (utils tc-utils)
         "rep-utils.rkt" "object-rep.rkt" "filter-rep.rkt" "free-variance.rkt"
         racket/match racket/list
         racket/contract
         racket/lazy-require
         (for-syntax racket/base syntax/parse))

(provide Mu-name:
         Poly-names: Poly-fresh:
         PolyDots-names:
         PolyRow-names: PolyRow-fresh:
         Type-seq
         Mu-unsafe: Poly-unsafe:
         PolyDots-unsafe:
         Mu? Poly? PolyDots? PolyRow?
         Filter? Object?
         Type/c Type/c?
         Values/c SomeValues/c
         Bottom?
         Poly-n
         PolyDots-n
         Class? Row? Row:
         free-vars*
         type-equal?
         remove-dups
         sub-t sub-f sub-o sub-pe
         (rename-out [Class:* Class:]
                     [Class* make-Class]
                     [Row* make-Row]
                     [Mu:* Mu:]
                     [Poly:* Poly:]
                     [PolyDots:* PolyDots:]
                     [PolyRow:* PolyRow:]
                     [Mu* make-Mu]
                     [Poly* make-Poly]
                     [PolyDots* make-PolyDots]
                     [PolyRow* make-PolyRow]
                     [Mu-body* Mu-body]
                     [Poly-body* Poly-body]
                     [PolyDots-body* PolyDots-body]
                     [PolyRow-body* PolyRow-body]))


;; Ugly hack - should use units
(lazy-require
  ("../types/union.rkt" (Un))
  ("../types/resolve.rkt" (resolve-app)))

(define name-table (make-weak-hasheq))

(define Type/c?
   (λ (e)
     (and (Type? e)
          (not (Scope? e))
          (not (arr? e))
          (not (fld? e))
          (not (Values? e))
          (not (ValuesDots? e))
          (not (AnyValues? e))
          (not (Result? e)))))

;; (or/c Type/c Values? Results?)
;; Anything that can be treated as a Values by sufficient expansion
(define Values/c?
   (λ (e)
     (and (Type? e)
          (not (Scope? e))
          (not (arr? e))
          (not (fld? e))
          (not (ValuesDots? e))
          (not (AnyValues? e)))))

(define Type/c (flat-named-contract 'Type Type/c?))
(define Values/c (flat-named-contract 'Values Values/c?))
(define Bottom?
  (match-lambda
    [(Union: (list)) #t]
    [else #f]))

;; Name = Symbol

;; Type is defined in rep-utils.rkt

;; t must be a Type
(def-type Scope ([t (or/c Type/c Scope?)]) [#:key (Type-key t)])

(define (scope-depth k)
  (flat-named-contract
   (format "Scope of depth ~a" k)
   (lambda (sc)
     (define (f k sc)
       (cond [(= 0 k) (Type/c? sc)]
             [(not (Scope? sc)) #f]
             [else (f (sub1 k) (Scope-t sc))]))
     (f k sc))))

;; this is ONLY used when a type error ocurrs
(def-type Error () [#:frees #f] [#:fold-rhs #:base])

;; de Bruijn indexes - should never appear outside of this file
;; bound type variables
;; i is an nat
(def-type B ([i natural-number/c]) [#:frees #f] [#:fold-rhs #:base])

;; free type variables
;; n is a Name
(def-type F ([n symbol?]) [#:frees (single-free-var n) empty-free-vars]
  [#:fold-rhs #:base])

;; Name, an indirection of a type through the environment
;;
;; interp.
;; A type name, potentially recursive or mutually recursive or pointing
;; to a type for a struct type
;; id is the name stored in the environment
;; deps are the other aliases this depends on, if any
;; args are the type parameters for this type (or #f if none)
;; struct? indicates if this maps to a struct type
(def-type Name ([id identifier?]
                [deps (listof identifier?)]
                [args (or/c #f (listof identifier?))]
                [struct? boolean?])
  [#:intern (hash-id id)] [#:frees #f] [#:fold-rhs #:base])

;; rator is a type
;; rands is a list of types
;; stx is the syntax of the pair of parens
(def-type App ([rator Type/c] [rands (listof Type/c)] [stx (or/c #f syntax?)])
  [#:intern (cons (Rep-seq rator) (map Rep-seq rands))]
  [#:frees (λ (f)
              (match rator 
                ((Name: n _ _ _)
                 (instantiate-frees n (map f rands)))
                (else (f (resolve-app rator rands stx)))))]

  [#:fold-rhs (*App (type-rec-id rator)
                    (map type-rec-id rands)
                    stx)])

;; left and right are Types
(def-type Pair ([left Type/c] [right Type/c]) [#:key 'pair])

;; dotted list -- after expansion, becomes normal Pair-based list type
(def-type ListDots ([dty Type/c] [dbound (or/c symbol? natural-number/c)])
  [#:frees (if (symbol? dbound)
               (free-vars-remove (free-vars* dty) dbound)
               (free-vars* dty))
           (if (symbol? dbound)
               (combine-frees (list (single-free-var dbound) (free-idxs* dty)))
               (free-idxs* dty))]
  [#:fold-rhs (*ListDots (type-rec-id dty) dbound)])

;; *mutable* pairs - distinct from regular pairs
;; left and right are Types
(def-type MPair ([left Type/c] [right Type/c])
  [#:frees (λ (f) (make-invariant (combine-frees (list (f left) (f right)))))]
  [#:key 'mpair])

;; elem is a Type
(def-type Vector ([elem Type/c])
  [#:frees (λ (f) (make-invariant (f elem)))]
  [#:key 'vector])

;; elems are all Types
(def-type HeterogeneousVector ([elems (listof Type/c)])
  [#:intern (map Rep-seq elems)]
  [#:frees (λ (f) (make-invariant (combine-frees (map f elems))))]
  [#:key 'vector]
  [#:fold-rhs (*HeterogeneousVector (map type-rec-id elems))])

;; elem is a Type
(def-type Box ([elem Type/c])
  [#:frees (λ (f) (make-invariant (f elem)))]
  [#:key 'box])

;; elem is a Type
(def-type Channel ([elem Type/c])
  [#:frees (λ (f) (make-invariant (f elem)))]
  [#:key 'channel])

;; elem is a Type
(def-type Async-Channel ([elem Type/c])
  [#:frees (λ (f) (make-invariant (f elem)))]
  [#:key 'async-channel])

;; elem is a Type
(def-type ThreadCell ([elem Type/c])
  [#:frees (λ (f) (make-invariant (f elem)))]
  [#:key 'thread-cell])

;; elem is a Type
(def-type Promise ([elem Type/c])
  [#:key 'promise])

;; elem is a Type
(def-type Ephemeron ([elem Type/c])
  [#:key 'ephemeron])

;; elem is a Type
(def-type CustodianBox ([elem Type/c])
  [#:key 'custodian-box])

;; elem is a Type
(def-type Set ([elem Type/c])
  [#:key 'set])

;; result is a Type
(def-type Evt ([result Type/c])
  [#:key #f])

;; name is a Symbol (not a Name)
;; contract is used when generating contracts from types
;; predicate is used to check (at compile-time) whether a value belongs
;; to that base type. This is used to check for subtyping between value
;; types and base types.
;; numeric determines if the type is a numeric type
(def-type Base ([name symbol?] [contract syntax?] [predicate procedure?] [numeric? boolean?])
  [#:frees #f] [#:fold-rhs #:base] [#:intern name]
  [#:key (if numeric?
             'number
             (case name
               [(Boolean) 'boolean]
               [(String) 'string]
               [(Symbol) 'symbol]
               [(Keyword) 'keyword]
               [else #f]))])

;; body is a Scope
(def-type Mu ([body (scope-depth 1)]) #:no-provide [#:frees (λ (f) (f body))]
  [#:fold-rhs (*Mu (*Scope (type-rec-id (Scope-t body))))]
  [#:key (Type-key body)])

;; n is how many variables are bound here
;; body is a Scope
(def-type Poly (n body) #:no-provide
  [#:contract (->i ([n natural-number/c]
                    [body (n) (scope-depth n)])
                   (#:syntax [stx (or/c #f syntax?)])
                   [result Poly?])]
  [#:frees (λ (f) (f body))]
  [#:fold-rhs (let ([body* (remove-scopes n body)])
                (*Poly n (add-scopes n (type-rec-id body*))))]
  [#:key (Type-key body)])

;; n is how many variables are bound here
;; there are n-1 'normal' vars and 1 ... var
;; body is a Scope
(def-type PolyDots (n body) #:no-provide
  [#:contract (->i ([n natural-number/c]
                    [body (n) (scope-depth n)])
                   (#:syntax [stx (or/c #f syntax?)])
                   [result PolyDots?])]
  [#:key (Type-key body)]
  [#:frees (λ (f) (f body))]
  [#:fold-rhs (let ([body* (remove-scopes n body)])
                (*PolyDots n (add-scopes n (type-rec-id body*))))])

;; interp. A row polymorphic function type
;; constraints are row absence constraints, represented
;; as a set for each of init, field, methods
(def-type PolyRow (constraints body) #:no-provide
  [#:contract (->i ([constraints (list/c list? list? list? list?)]
                    [body (scope-depth 1)])
                   (#:syntax [stx (or/c #f syntax?)])
                   [result PolyRow?])]
  [#:frees (λ (f) (f body))]
  [#:fold-rhs (let ([body* (remove-scopes 1 body)])
                (*PolyRow constraints
                          (add-scopes 1 (type-rec-id body*))))]
  [#:key (Type-key body)])

;; pred : identifier
(def-type Opaque ([pred identifier?])
  [#:intern (hash-id pred)] [#:frees #f] [#:fold-rhs #:base] [#:key pred])

;; kw : keyword?
;; ty : Type
;; required? : Boolean
(def-type Keyword ([kw keyword?] [ty Type/c] [required? boolean?])
  [#:frees (λ (f) (f ty))]
  [#:fold-rhs (*Keyword kw (type-rec-id ty) required?)])

(def-type Result ([t Type/c] [f FilterSet?] [o Object?])
  [#:frees (λ (frees) (combine-frees (map frees (list t f o))))]
  [#:fold-rhs (*Result (type-rec-id t) (filter-rec-id f) (object-rec-id o))])

(def-type Values ([rs (listof Result?)])
  [#:intern (map Rep-seq rs)]
  [#:frees (λ (f) (combine-frees (map f rs)))]
  [#:fold-rhs (*Values (map type-rec-id rs))])


(def-type AnyValues ([f Filter/c])
  [#:fold-rhs #:base])

(def-type ValuesDots ([rs (listof Result?)] [dty Type/c] [dbound (or/c symbol? natural-number/c)])
  [#:intern (list (map Rep-seq rs) (Rep-seq dty) dbound)]
  [#:frees (if (symbol? dbound)
               (free-vars-remove (combine-frees (map free-vars* (cons dty rs))) dbound)
               (combine-frees (map free-vars* (cons dty rs))))
           (if (symbol? dbound)
               (combine-frees (cons (single-free-var dbound) 
                                    (map free-idxs* (cons dty rs))))
               (combine-frees (map free-idxs* (cons dty rs))))]
  [#:fold-rhs (*ValuesDots (map type-rec-id rs) (type-rec-id dty) dbound)])

(define SomeValues/c (or/c Values? AnyValues? ValuesDots?))

;; arr is NOT a Type
(def-type arr ([dom (listof Type/c)]
               [rng SomeValues/c]
               [rest (or/c #f Type/c)]
               [drest (or/c #f (cons/c Type/c (or/c natural-number/c symbol?)))]
               [kws (listof Keyword?)])
  [#:intern (list (map Rep-seq dom) (Rep-seq rng) (and rest (Rep-seq rest))
                  (and drest (cons (Rep-seq (car drest)) (cdr drest)))
                  (map Rep-seq kws))]
  [#:frees (combine-frees
            (append (map (compose flip-variances free-vars*)
                         (append (if rest (list rest) null)
                                 (map Keyword-ty kws)
                                 dom))
                    (match drest
                      [(cons t (? symbol? bnd))
                       (list (free-vars-remove (flip-variances (free-vars* t)) bnd))]
                      [(cons t _)
                       (list (flip-variances (free-vars* t)))]
                      [_ null])
                    (list (free-vars* rng))))
           (combine-frees
            (append (map (compose flip-variances free-idxs*)
                         (append (if rest (list rest) null)
                                 (map Keyword-ty kws)
                                 dom))
                    (match drest
                      [(cons t (? symbol? bnd))
                       (list (single-free-var bnd Contravariant)
                             (flip-variances (free-idxs* t)))]
                      [(cons t _)
                       (list (flip-variances (free-idxs* t)))]
                      [_ null])
                    (list (free-idxs* rng))))]
  [#:fold-rhs (*arr (map type-rec-id dom)
                    (type-rec-id rng)
                    (and rest (type-rec-id rest))
                    (and drest (cons (type-rec-id (car drest)) (cdr drest)))
                    (map type-rec-id kws))])

;; arities : Listof[arr]
(def-type Function ([arities (listof arr?)])
  [#:intern (map Rep-seq arities)]
  [#:key 'procedure]
  [#:frees (λ (f) (combine-frees (map f arities)))]
  [#:fold-rhs (*Function (map type-rec-id arities))])


(def-type fld ([t Type/c] [acc identifier?] [mutable? boolean?])
  [#:frees (λ (f) (if mutable? (make-invariant (f t)) (f t)))]
  [#:fold-rhs (*fld (type-rec-id t) acc mutable?)]
  [#:intern (list (Rep-seq t) (hash-id acc) mutable?)])

;; name : identifier
;; parent : Struct
;; flds : Listof[fld]
;; proc : Function Type
;; poly? : is this type polymorphically variant
;;         If not, then the predicate is enough for higher order checks
;; pred-id : identifier for the predicate of the struct
;; acc-ids : names of the accessors
;; maker-id : name of the constructor
(def-type Struct ([name identifier?]
                  [parent (or/c #f Struct?)]
                  [flds (listof fld?)]
                  [proc (or/c #f Function?)]
                  [poly? boolean?]
                  [pred-id identifier?])
  [#:intern (list (hash-id name)
                  (hash-id pred-id)
                  (and parent (Rep-seq parent))
                  (map Rep-seq flds)
                  (and proc (Rep-seq proc)))]
  [#:frees (λ (f) (combine-frees (map f (append (if proc (list proc) null)
                                                (if parent (list parent) null)
                                                flds))))]
  [#:fold-rhs (*Struct name
                       (and parent (type-rec-id parent))
                       (map type-rec-id flds)
                       (and proc (type-rec-id proc))
                       poly?
                       pred-id)]
  ;; This should eventually be based on understanding of struct properties.
  [#:key '(struct procedure)])

;; A structure type descriptor
(def-type StructTypeTop () [#:fold-rhs #:base] [#:key 'struct-type])
(def-type StructType ([s (or/c F? B? Struct?)]) [#:key 'struct-type])

;; the supertype of all of these values
(def-type BoxTop () [#:fold-rhs #:base] [#:key 'box])
(def-type ChannelTop () [#:fold-rhs #:base] [#:key 'channel])
(def-type Async-ChannelTop () [#:fold-rhs #:base] [#:key 'async-channel])
(def-type VectorTop () [#:fold-rhs #:base] [#:key 'vector])
(def-type HashtableTop () [#:fold-rhs #:base] [#:key 'hash])
(def-type MPairTop () [#:fold-rhs #:base] [#:key 'mpair])
(def-type StructTop ([name Struct?]) [#:key 'struct])
(def-type ThreadCellTop () [#:fold-rhs #:base] [#:key 'thread-cell])
(def-type Prompt-TagTop () [#:fold-rhs #:base] [#:key 'prompt-tag])
(def-type Continuation-Mark-KeyTop ()
  [#:fold-rhs #:base] [#:key 'continuation-mark-key])

;; v : Racket Value
(def-type Value (v) [#:frees #f] [#:fold-rhs #:base]
  [#:key (cond [(or (eq? v 0) (eq? v 1)) 'number]
               ;; other numbers don't work with the optimizations in subtype.rkt
               ;; which assume that unions of numbers are subtyped in simple ways
               [(boolean? v) 'boolean]
               [(null? v) 'null]
               [else #f])])

;; elems : Listof[Type]
(def-type Union ([elems (and/c (listof Type/c)
                               (lambda (es)
                                 (or (null? es)
                                     (let-values ([(sorted? k)
                                                   (for/fold ([sorted? #t]
                                                              [last (car es)])
                                                       ([e (cdr es)])
                                                     (values
                                                      (and sorted? (type<? last e))
                                                      e))])
                                       sorted?))))])
  [#:intern (map Rep-seq elems)]
  [#:frees (λ (f) (combine-frees (map f elems)))]
  [#:fold-rhs (apply Un (map type-rec-id elems))]
  [#:key 
   (let ()
     (define d
       (let loop ([ts elems] [res null])
         (cond [(null? ts) res]
               [else
                (define k (Type-key (car ts)))
                (cond [(not k) (list #f)]
                      [(pair? k) (loop (cdr ts) (append k res))]
                      [else (loop (cdr ts) (cons k res))])])))
     (define d* (remove-duplicates d))
     (if (and (pair? d*) (null? (cdr d*))) (car d*) d*))])

(def-type Univ () [#:frees #f] [#:fold-rhs #:base])

;; in : Type
;; out : Type
(def-type Param ([in Type/c] [out Type/c])
  [#:key 'procedure]
  [#:frees (λ (f) (combine-frees (list (f out) (flip-variances (f in)))))])

;; key : Type
;; value : Type
(def-type Hashtable ([key Type/c] [value Type/c]) [#:key 'hash]
  [#:frees (λ (f) (combine-frees (list (make-invariant (f key)) (make-invariant (f value)))))])

(def-type Refinement ([parent Type/c] [pred identifier?])
  [#:key (Type-key parent)]
  [#:intern (list (Rep-seq parent) (hash-id pred))]
  [#:fold-rhs (*Refinement (type-rec-id parent) pred)]
  [#:frees (λ (f) (f parent))])


;; t : Type
(def-type Syntax ([t Type/c]) [#:key 'syntax])

;; A Row used in type instantiation
;; For now, this should not appear in user code. It's used
;; internally to perform row instantiations and to represent
;; class types.
;;
;; invariant: all clauses are sorted by the key name
(def-type Row ([inits (listof (list/c symbol? Type/c boolean?))]
               [fields (listof (list/c symbol? Type/c))]
               [methods (listof (list/c symbol? Function?))]
               [augments (listof (list/c symbol? Function?))]
               [init-rest (or/c Type/c #f)])
  #:no-provide
  [#:frees (λ (f) (combine-frees
                   (map f (append (map cadr inits)
                                  (map cadr fields)
                                  (map cadr methods)
                                  (map cadr augments)
                                  (if init-rest (list init-rest) null)))))]
  [#:fold-rhs (match (list inits fields methods augments init-rest)
                [(list
                  (list (list init-names init-tys reqd) ___)
                  (list (list fname fty) ___)
                  (list (list mname mty) ___)
                  (list (list aname aty) ___)
                  init-rest)
                 (*Row
                  (map list
                       init-names
                       (map type-rec-id init-tys)
                       reqd)
                  (map list fname (map type-rec-id fty))
                  (map list mname (map type-rec-id mty))
                  (map list aname (map type-rec-id aty))
                  (if init-rest (type-rec-id init-rest) #f))])])

;; Supertype of all Class types, cannot instantiate
;; or subclass these
(def-type ClassTop () [#:fold-rhs #:base] [#:key 'class])

;; row-ext : Option<(U F B Row)>
;; row     : Row
;;
;; interp. The first field represents a row extension
;;         The second field represents the concrete row
;;         that the class starts with
;;
(def-type Class ([row-ext (or/c #f F? B? Row?)]
                 [row Row?])
  #:no-provide
  [#:frees (λ (f) (combine-frees
                   ;; FIXME: is this correct?
                   `(,@(or (and (F? row-ext) (list (f row-ext)))
                           '())
                     ,(f row))))]
  [#:key 'class]
  [#:fold-rhs (match (list row-ext row)
                [(list row-ext row)
                 (*Class
                  (and row-ext (type-rec-id row-ext))
                  (type-rec-id row))])])

;; cls : Class
(def-type Instance ([cls Type/c]) [#:key 'instance])

;; sequences
;; includes lists, vectors, etc
;; tys : sequence produces this set of values at each step
(def-type Sequence ([tys (listof Type/c)])
  [#:intern (map Rep-seq tys)]
  [#:frees (λ (f) (combine-frees (map f tys)))]
  [#:key #f] [#:fold-rhs (*Sequence (map type-rec-id tys))])

(def-type Future ([t Type/c]) [#:key 'future])

;; body: the type of the body
;; handler: the type of the prompt handler
;;   prompts with this tag will return a union of `body` 
;;   and the codomains of `handler`
(def-type Prompt-Tagof ([body Type/c] [handler Type/c])
  [#:frees (λ (f) (combine-frees (list (make-invariant (f body))
                                       (make-invariant (f handler)))))]
  [#:key 'prompt-tag])

;; value: the type of allowable values
(def-type Continuation-Mark-Keyof ([value Type/c])
  [#:frees (λ (f) (make-invariant (f value)))]
  [#:key 'continuation-mark-key])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove-dups: List[Type] -> List[Type]
;; removes duplicate types from a SORTED list
(define/cond-contract (remove-dups types)
  ((listof Rep?) . -> . (listof Rep?))
  (cond [(null? types) types]
        [(null? (cdr types)) types]
        [(type-equal? (car types) (cadr types)) (remove-dups (cdr types))]
        [else (cons (car types) (remove-dups (cdr types)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (add-scopes n t)
  (if (zero? n) t
      (add-scopes (sub1 n) (*Scope t))))

(define (remove-scopes n sc)
  (if (zero? n)
      sc
      (match sc
        [(Scope: sc*) (remove-scopes (sub1 n) sc*)]
        [_ (int-err "Tried to remove too many scopes: ~a" sc)])))


(define ((sub-f st) e)
  (filter-case (#:Type st
                #:Filter (sub-f st)
                #:PathElem (sub-pe st))
               e))


(define ((sub-o st) e)
  (object-case (#:Type st
                #:Object (sub-o st)
                #:PathElem (sub-pe st))
               e))

(define ((sub-pe st) e)
  (pathelem-case (#:Type st
                         #:PathElem (sub-pe st))
                 e))

(define ((sub-t st) e)
  (type-case (#:Type st
              #:Filter (sub-f st))
              e))


;; abstract-many : Names Type -> Scope^n
;; where n is the length of names
(define (abstract-many names ty)
  ;; mapping : dict[Type -> Natural]
  (define (nameTo mapping type)
    (let loop ([outer 0] [ty type])
      (define (sb t) (loop outer t))
      ;; transform : Name (Integer -> a) a -> a
      ;; apply `mapping` to `name*`, returning `default` if it's not there
      ;; use `f` to wrap the result
      ;; note that this takes into account the value of `outer`
      (define (transform name* f default)
        (cond [(assq name* mapping)
               => (λ (pr)
                    (f (+ (cdr pr) outer)))]
              [else default]))
      (type-case
        (#:Type sb #:Filter (sub-f sb) #:Object (sub-o sb))
       ty
       [#:F name* (transform name* *B ty)]
       ;; necessary to avoid infinite loops
       [#:Union elems (*Union (remove-dups (sort (map sb elems) type<?)))]
       ;; functions
       [#:arr dom rng rest drest kws
              (*arr (map sb dom)
                    (sb rng)
                    (if rest (sb rest) #f)
                    (if drest
                        (cons (sb (car drest))
                              (let ([c (cdr drest)])
                                 (transform c values c)))
                        #f)
                    (map sb kws))]
       [#:ValuesDots rs dty dbound
              (*ValuesDots (map sb rs)
                           (sb dty)
                           (transform dbound values dbound))]
       [#:ListDots dty dbound
                   (*ListDots (sb dty)
                              (transform dbound values dbound))]
       [#:Mu (Scope: body) (*Mu (*Scope (loop (add1 outer) body)))]
       [#:PolyRow constraints body*
                  (let ([body (remove-scopes 1 body*)])
                    (*PolyRow constraints
                              (add-scopes 1 (loop (+ 1 outer) body))))]
       [#:PolyDots n body*
                   (let ([body (remove-scopes n body*)])
                     (*PolyDots n (add-scopes n (loop (+ n outer) body))))]
       [#:Poly n body*
               (let ([body (remove-scopes n body*)])
                 (*Poly n (add-scopes n (loop (+ n outer) body))))])))
  (define n (length names))
  (define mapping (for/list ([nm (in-list names)]
                             [i (in-range n 0 -1)])
                    (cons nm (sub1 i))))
  (add-scopes n (nameTo mapping ty)))

;; instantiate-many : List[Type] Scope^n -> Type
;; where n is the length of types
;; all of the types MUST be Fs
(define (instantiate-many images sc)
  ;; mapping : dict[Natural -> Type]
  (define (replace mapping type)
    (let loop ([outer 0] [ty type])
      ;; transform : Integer (Name -> a) a -> a
      ;; apply `mapping` to `idx`, returning `default` if it's not there
      ;; use `f` to wrap the result
      ;; note that this takes into account the value of `outer`
      (define (transform idx f default)
        (cond [(assf (lambda (v) (eqv? (+ v outer) idx)) mapping)
               => (lambda (pr) (f (cdr pr)))]
              [else default]))
      (define (sb t) (loop outer t))
      (define sf (sub-f sb))
      (type-case
       (#:Type sb #:Filter sf #:Object (sub-o sb))
       ty
       [#:B idx (transform idx values ty)]
       ;; necessary to avoid infinite loops
       [#:Union elems (*Union (remove-dups (sort (map sb elems) type<?)))]
       ;; functions
       [#:arr dom rng rest drest kws
              (*arr (map sb dom)
                    (sb rng)
                    (if rest (sb rest) #f)
                    (if drest
                        (cons (sb (car drest))
                              (transform (cdr drest) F-n (cdr drest)))
                        #f)
                    (map sb kws))]
       [#:ValuesDots rs dty dbound
                     (*ValuesDots (map sb rs)
                                  (sb dty)
                                  (transform dbound F-n dbound))]
       [#:ListDots dty dbound
                   (*ListDots (sb dty)
                              (transform dbound F-n dbound))]
       [#:Mu (Scope: body) (*Mu (*Scope (loop (add1 outer) body)))]
       [#:PolyRow constraints body*
                  (let ([body (remove-scopes 1 body*)])
                    (*PolyRow constraints (add-scopes 1 (loop (+ 1 outer) body))))]
       [#:PolyDots n body*
                   (let ([body (remove-scopes n body*)])
                     (*PolyDots n (add-scopes n (loop (+ n outer) body))))]
       [#:Poly n body*
               (let ([body (remove-scopes n body*)])
                 (*Poly n (add-scopes n (loop (+ n outer) body))))])))
  (define n (length images))
  (define mapping (for/list ([img (in-list images)]
                             [i (in-range n 0 -1)])
                    (cons (sub1 i) img)))
  (replace mapping (remove-scopes n sc)))

(define (abstract name ty)
  (abstract-many (list name) ty))

(define (instantiate type sc)
  (instantiate-many (list type) sc))

;; the 'smart' constructor
(define (Mu* name body)
  (let ([v (*Mu (abstract name body))])
    (hash-set! name-table v name)
    v))

;; the 'smart' destructor
(define (Mu-body* name t)
  (match t
    [(Mu: scope)
     (instantiate (*F name) scope)]))

;; the 'smart' constructor
;;
;; Corresponds to closing a type in locally nameless representation
;; (turns free `names` into bound De Bruijn vars)
;; Also keeps track of the original name in a table to recover names
;; for debugging or to correlate with surface syntax
;;
;; Provide #:original-names if the names that you are closing off
;; are *different* from the names you want recorded in the table.
;;
;; list<symbol> type #:original-names list<symbol> -> type
;;
(define (Poly* names body #:original-names [orig names])
  (if (null? names) body
      (let ([v (*Poly (length names) (abstract-many names body))])
        (hash-set! name-table v orig)
        v)))

;; the 'smart' destructor
(define (Poly-body* names t)
  (match t
    [(Poly: n scope)
     (unless (= (length names) n)
       (int-err "Wrong number of names: expected ~a got ~a" n (length names)))
     (instantiate-many (map *F names) scope)]))

;; the 'smart' constructor
(define (PolyDots* names body)
  (if (null? names) body
      (let ([v (*PolyDots (length names) (abstract-many names body))])
        (hash-set! name-table v names)
        v)))

;; the 'smart' destructor
(define (PolyDots-body* names t)
  (match t
    [(PolyDots: n scope)
     (unless (= (length names) n)
       (int-err "Wrong number of names: expected ~a got ~a" n (length names)))
     (instantiate-many (map *F names) scope)]))

;; Constructor and destructor for row polymorphism
;;
;; Note that while `names` lets you specify multiple names, it's
;; expected that row polymorphic types only bind a single name at
;; a time. This may change in the future.
;;
(define (PolyRow* names constraints body #:original-names [orig names])
  (let ([v (*PolyRow constraints (abstract-many names body))])
    (hash-set! name-table v orig)
    v))

(define (PolyRow-body* names t)
  (match t
    [(PolyRow: constraints scope)
     (instantiate-many (map *F names) scope)]))

(print-struct #t)

(define-match-expander Mu-unsafe:
  (lambda (stx)
    (syntax-case stx ()
      [(_ bp) #'(? Mu? (app (lambda (t) (Scope-t (Mu-body t))) bp))])))

(define-match-expander Poly-unsafe:
  (lambda (stx)
    (syntax-case stx ()
      [(_ n bp) #'(? Poly? (app (lambda (t) (list (Poly-n t) (Poly-body t))) (list n bp)))])))

(define-match-expander PolyDots-unsafe:
  (lambda (stx)
    (syntax-case stx ()
      [(_ n bp) #'(? PolyDots? (app (lambda (t) (list (PolyDots-n t) (PolyDots-body t))) (list n bp)))])))

(define-match-expander Mu:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ np bp)
       #'(? Mu?
            (app (lambda (t) (let ([sym (gensym)])
                               (list sym (Mu-body* sym t))))
                 (list np bp)))])))

(define-match-expander Mu-name:
  (lambda (stx)
    (syntax-case stx ()
      [(_ np bp)
       #'(? Mu?
            (app (lambda (t) (let ([sym (hash-ref name-table t (lambda _ (gensym)))])
                               (list sym (Mu-body* sym t))))
                 (list np bp)))])))

;; These match expanders correspond to opening up a type in
;; locally nameless representation. When the type is opened,
;; the nameless bound variables are replaced with free
;; variables with names.
;;
;; This match expander wraps the smart constructor
;; names are generated with gensym
(define-match-expander Poly:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps bp)
       #'(? Poly?
            (app (lambda (t)
                   (let* ([n (Poly-n t)]
                          [syms (build-list n (lambda _ (gensym)))])
                     (list syms (Poly-body* syms t))))
                 (list nps bp)))])))

;; This match expander uses the names from the hashtable
(define-match-expander Poly-names:
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps bp)
       #'(? Poly?
            (app (lambda (t)
                   (let* ([n (Poly-n t)]
                          [syms (hash-ref name-table t (lambda _ (build-list n (lambda _ (gensym)))))])
                     (list syms (Poly-body* syms t))))
                 (list nps bp)))])))

;; Helper for fresh match expanders below, creates a
;; fresh name that prints the same as the original
(define (fresh-name sym)
  (string->uninterned-symbol (symbol->string sym)))

;; This match expander creates new fresh names for exploring the body
;; of the polymorphic type. When lexical scoping of type variables is a concern, you
;; should use this form.
(define-match-expander Poly-fresh:
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps freshp bp)
       #'(? Poly?
            (app (lambda (t)
                   (let* ([n (Poly-n t)]
                          [syms (hash-ref name-table t (lambda _ (build-list n (lambda _ (gensym)))))]
                          [fresh-syms (map fresh-name syms)])
                     (list syms fresh-syms (Poly-body* fresh-syms t))))
                 (list nps freshp bp)))])))

;; This match expander wraps the smart constructor
;; names are generated with gensym
(define-match-expander PolyDots:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps bp)
       #'(? PolyDots?
            (app (lambda (t)
                   (let* ([n (PolyDots-n t)]
                          [syms (build-list n (lambda _ (gensym)))])
                     (list syms (PolyDots-body* syms t))))
                 (list nps bp)))])))

;; This match expander uses the names from the hashtable
(define-match-expander PolyDots-names:
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps bp)
       #'(? PolyDots?
            (app (lambda (t)
                   (let* ([n (PolyDots-n t)]
                          [syms (hash-ref name-table t (lambda _ (build-list n (lambda _ (gensym)))))])
                     (list syms (PolyDots-body* syms t))))
                 (list nps bp)))])))

(define-match-expander PolyRow:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps constrp bp)
       #'(? PolyRow?
            (app (lambda (t)
                   (define sym (gensym))
                   (list (list sym)
                         (PolyRow-constraints t)
                         (PolyRow-body* (list sym) t)))
                 (list nps constrp bp)))])))

(define-match-expander PolyRow-names:
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps constrp bp)
       #'(? PolyRow?
            (app (lambda (t)
                   (define syms (hash-ref name-table t (λ _ (list (gensym)))))
                   (list syms
                         (PolyRow-constraints t)
                         (PolyRow-body* syms t)))
                 (list nps constrp bp)))])))

(define-match-expander PolyRow-fresh:
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps freshp constrp bp)
       #'(? PolyRow?
            (app (lambda (t)
                   (define syms (hash-ref name-table t (λ _ (list (gensym)))))
                   (define fresh-syms (list (gensym (car syms))))
                   (list syms fresh-syms
                         (PolyRow-constraints t)
                         (PolyRow-body* fresh-syms t)))
                 (list nps freshp constrp bp)))])))

;; Row*
;; This is a custom constructor for Row types
;; Sorts all clauses by the key (the clause name)
(define (Row* inits fields methods augments init-rest)
  (*Row inits
        (sort-row-clauses fields)
        (sort-row-clauses methods)
        (sort-row-clauses augments)
        init-rest))

;; Class*
;; This is a custom constructor for Class types that
;; doesn't require writing make-Row everywhere
(define/cond-contract (Class* row-var inits fields methods augments init-rest)
  (-> (or/c F? B? Row? #f)
      (listof (list/c symbol? Type/c boolean?))
      (listof (list/c symbol? Type/c))
      (listof (list/c symbol? Function?))
      (listof (list/c symbol? Function?))
      (or/c Type/c #f)
      Class?)
  (*Class row-var (Row* inits fields methods augments init-rest)))

;; Class:*
;; This match expander replaces the built-in matching with
;; a version that will merge the members inside the substituted row
;; with the existing fields.

;; helper function for the expansion of Class:*
;; just does the merging
(define (merge-class/row class-type)
  (define row (Class-row-ext class-type))
  (define class-row (Class-row class-type))
  (define inits (Row-inits class-row))
  (define fields (Row-fields class-row))
  (define methods (Row-methods class-row))
  (define augments (Row-augments class-row))
  (define init-rest (Row-init-rest class-row))
  (cond [(and row (Row? row))
         (define row-inits (Row-inits row))
         (define row-fields (Row-fields row))
         (define row-methods (Row-methods row))
         (define row-augments (Row-augments row))
         (define row-init-rest (Row-init-rest row))
         (list row
               ;; Init types from a mixin go at the start, since
               ;; mixins only add inits at the start
               (append row-inits inits)
               ;; FIXME: instead of sorting here every time
               ;;        the match expander is called, the row
               ;;        fields should be merged on substitution
               (sort-row-clauses (append fields row-fields))
               (sort-row-clauses (append methods row-methods))
               (sort-row-clauses (append augments row-augments))
               ;; The class type's existing init-rest types takes
               ;; precedence since it's the one that was already assumed
               ;; (say, in a mixin type's domain). The mismatch will
               ;; be caught by application type-checking later.
               (if init-rest init-rest row-init-rest))]
        [else (list row inits fields methods augments init-rest)]))

;; sorts the given field of a Row by the member name
(define (sort-row-clauses clauses)
  (sort clauses symbol<? #:key car))

(define-match-expander Class:*
  (λ (stx)
    (syntax-case stx ()
      [(_ row-pat inits-pat fields-pat methods-pat augments-pat init-rest-pat)
       #'(? Class?
            (app merge-class/row
                 (list row-pat inits-pat fields-pat
                       methods-pat augments-pat init-rest-pat)))])))

