#lang scheme/base
(require "../utils/utils.rkt")

(require (utils tc-utils)
	 "rep-utils.rkt" "object-rep.rkt" "filter-rep.rkt" "free-variance.rkt"
         mzlib/trace racket/match mzlib/etc
         scheme/contract
         (for-syntax scheme/base syntax/parse))

(define name-table (make-weak-hasheq))

(define Type/c?
   (λ (e)
     (and (Type? e)
          (not (Scope? e))
          (not (arr? e))
          (not (fld? e))
          (not (Values? e))
          (not (ValuesDots? e))
          (not (Result? e)))))

(define Type/c (flat-named-contract 'Type Type/c?))

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
(def-type F ([n symbol?]) [#:frees (make-immutable-hasheq (list (cons n Covariant))) #hasheq()] [#:fold-rhs #:base])

;; id is an Identifier
(def-type Name ([id identifier?]) [#:intern (hash-id id)] [#:frees #f] [#:fold-rhs #:base])

;; rator is a type
;; rands is a list of types
;; stx is the syntax of the pair of parens
(def-type App ([rator Type/c] [rands (listof Type/c)] [stx (or/c #f syntax?)])
  [#:intern (cons (Rep-seq rator) (map Rep-seq rands))]
  [#:frees (λ (f) (combine-frees (map f (cons rator rands))))]
  [#:fold-rhs (*App (type-rec-id rator)
                    (map type-rec-id rands)
                    stx)])

(define (get-variances t num-rands)
  (match t
    [(Name: v) (error 'fail)]
    [(Poly: n scope)
     (let ([t (free-idxs* scope)])
       (for/list ([i (in-range n)])
         (hash-ref t i)))]
    [(PolyDots: n scope)
     (let ([t (free-idxs* scope)]
           [base-count (sub1 n)]
           [extras (max 0 (- n num-rands))])
       (append
        ;; variances of the fixed arguments
        (for/list ([i (in-range base-count)])
          (hash-ref t i))
        ;; variance of the dotted arguments
        (for/list ([i (in-range extras)])
          (hash-ref t n))))]))

(define (apply-variance v tbl)
  (evcase v
    [(Constant) (make-constant tbl)]
    [(Covariant) tbl]
    [(Invariant) (make-invariant tbl)]
    [(Contravariant) (flip-variances tbl)]))

;; left and right are Types
(def-type Pair ([left Type/c] [right Type/c]) [#:key 'pair])

;; dotted list -- after expansion, becomes normal Pair-based list type
(def-type ListDots ([dty Type/c] [dbound (or/c symbol? natural-number/c)])
  [#:frees (if (symbol? dbound)
               (hash-remove (free-vars* dty) dbound)
               (free-vars* dty))
           (if (symbol? dbound)
               (combine-frees (list (make-immutable-hasheq (list (cons dbound Covariant))) (free-idxs* dty)))
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
(def-type HeterogenousVector ([elems (listof Type/c)])
  [#:frees (λ (f) (make-invariant (combine-frees (map f elems))))]
  [#:key 'vector]
  [#:fold-rhs (*HeterogenousVector (map type-rec-id elems))])

;; elem is a Type
(def-type Box ([elem Type/c])
  [#:frees (λ (f) (make-invariant (f elem)))]
  [#:key 'box])

;; elem is a Type
(def-type Channel ([elem Type/c])
  [#:frees (λ (f) (make-invariant (f elem)))]
  [#:key 'channel])

;; elem is a Type
(def-type ThreadCell ([elem Type/c])
  [#:frees (λ (f) (make-invariant (f elem)))]
  [#:key 'thread-cell])


;; elem is a Type
(def-type Ephemeron ([elem Type/c])
  [#:key 'ephemeron])

;; elem is a Type
(def-type CustodianBox ([elem Type/c])
  [#:key 'custodian-box])



;; elem is a Type
(def-type Set ([elem Type/c])
  [#:key 'set])




;; name is a Symbol (not a Name)
;; contract is used when generating contracts from types
;; predicate is used to check (at compile-time) whether a value belongs
;; to that base type. This is used to check for subtyping between value
;; types and base types.
;; marshaled has to be a syntax object that refers to the base type
;; being created. this allows us to avoid reconstructing the base type
;; when using it from its marshaled representation
(def-type Base ([name symbol?] [contract syntax?] [predicate procedure?] [marshaled syntax?] [numeric? boolean?])
  [#:frees #f] [#:fold-rhs #:base] [#:intern name]
  [#:key (if numeric?
             'number
             (case name
               [(Number Integer) 'number]
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

;; pred : identifier
;; cert : syntax certifier
(def-type Opaque ([pred identifier?] [cert procedure?])
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
  [#:frees (λ (f) (combine-frees (map f rs)))]
  [#:fold-rhs (*Values (map type-rec-id rs))])

(def-type ValuesDots ([rs (listof Result?)] [dty Type/c] [dbound (or/c symbol? natural-number/c)])
  [#:frees (if (symbol? dbound)
               (hash-remove (combine-frees (map free-vars* (cons dty rs))) dbound)
               (combine-frees (map free-vars* (cons dty rs))))
           (if (symbol? dbound)
               (combine-frees (cons (make-immutable-hasheq (list (cons dbound Covariant)))
                                    (map free-idxs* (cons dty rs))))
               (combine-frees (map free-idxs* (cons dty rs))))]
  [#:fold-rhs (*ValuesDots (map type-rec-id rs) (type-rec-id dty) dbound)])

;; arr is NOT a Type
(def-type arr ([dom (listof Type/c)]
               [rng (or/c Values? ValuesDots?)]
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
                       (list (hash-remove (flip-variances (free-vars* t)) bnd))]
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
                       (list (make-immutable-hasheq (list (cons bnd Contravariant)))
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

;; top-arr is the supertype of all function types
(def-type top-arr () [#:fold-rhs #:base])

(define arr/c (or/c top-arr? arr?))

;; arities : Listof[arr]
(def-type Function ([arities (listof arr/c)])
  [#:key 'procedure]
  [#:frees (λ (f) (combine-frees (map f arities)))]
  [#:fold-rhs (*Function (map type-rec-id arities))])


(def-type fld ([t Type/c] [acc identifier?] [mutable? boolean?])
  [#:frees (λ (f) (if mutable? (make-invariant (f t)) (f t)))]
  [#:fold-rhs (*fld (type-rec-id t) acc mutable?)]
  [#:intern (list t (hash-id acc) mutable?)])

;; name : identifier
;; parent : Struct
;; flds : Listof[fld]
;; proc : Function Type
;; poly? : is this a polymorphic type?
;; pred-id : identifier for the predicate of the struct
;; cert : syntax certifier for pred-id
;; acc-ids : names of the accessors
;; maker-id : name of the constructor
(def-type Struct ([name identifier?]
                  [parent (or/c #f Struct? Name?)]
                  [flds (listof fld?)]
                  [proc (or/c #f Function?)]
                  [poly? (or/c #f (listof symbol?))]
                  [pred-id identifier?]
                  [cert procedure?]
                  [maker-id identifier?])
  [#:intern (list name (and parent (Rep-seq parent)) (map Rep-seq flds) (and proc (Rep-seq proc)))]
  [#:frees (λ (f) (combine-frees (map f (append (if proc (list proc) null)
                                                (if parent (list parent) null)
                                                flds))))]
  [#:fold-rhs (*Struct name
                       (and parent (type-rec-id parent))
                       (map type-rec-id flds)
                       (and proc (type-rec-id proc))
                       poly?
                       pred-id
                       cert
                       maker-id)]
  [#:key 'struct])

;; A structure type descriptor
;; s : struct
(def-type StructType ([s Struct?]) [#:key 'struct-type])

;; the supertype of all of these values
(def-type BoxTop () [#:fold-rhs #:base] [#:key 'box])
(def-type ChannelTop () [#:fold-rhs #:base] [#:key 'channel])
(def-type VectorTop () [#:fold-rhs #:base] [#:key 'vector])
(def-type HashtableTop () [#:fold-rhs #:base] [#:key 'hash])
(def-type MPairTop () [#:fold-rhs #:base] [#:key 'mpair])
(def-type StructTop ([name Struct?]) [#:key 'struct])
(def-type ThreadCellTop () [#:fold-rhs #:base] [#:key 'thread-cell])

;; v : Scheme Value
(def-type Value (v) [#:frees #f] [#:fold-rhs #:base] [#:key (cond [(number? v) 'number]
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
  [#:frees (λ (f) (combine-frees (map f elems)))]
  [#:fold-rhs ((get-union-maker) (map type-rec-id elems))]
  [#:key (let loop ([res null] [ts elems])
           (if (null? ts) res
               (let ([k (Type-key (car ts))])
                 (cond [(pair? k) (loop (append k res) (cdr ts))]
                       [k (loop (cons k res) (cdr ts))]
                       [else #f]))))])

(def-type Univ () [#:frees #f] [#:fold-rhs #:base])

;; in : Type
;; out : Type
(def-type Param ([in Type/c] [out Type/c])
  [#:key 'parameter]
  [#:frees (λ (f) (combine-frees (list (f out) (flip-variances (f in)))))])

;; key : Type
;; value : Type
(def-type Hashtable ([key Type/c] [value Type/c]) [#:key 'hash]
  [#:frees (λ (f) (combine-frees (list (make-invariant (f key)) (make-invariant (f value)))))])

(def-type Refinement ([parent Type/c] [pred identifier?] [cert certifier?])
  [#:key (Type-key parent)]
  [#:intern (list (Rep-seq parent) (hash-id pred))]
  [#:fold-rhs (*Refinement (type-rec-id parent) pred cert)]
  [#:frees (λ (f) (f parent))])


;; t : Type
(def-type Syntax ([t Type/c]) [#:key 'syntax])

;; pos-flds  : (Listof Type)
;; name-flds : (Listof (Tuple Symbol Type Boolean))
;; methods   : (Listof (Tuple Symbol Function))
(def-type Class ([pos-flds (listof Type/c)]
                 [name-flds (listof (list/c symbol? Type/c boolean?))]
                 [methods (listof (list/c symbol? Function?))])
  [#:frees (λ (f) (combine-frees
                   (map f (append pos-flds
                                  (map cadr name-flds)
                                  (map cadr methods)))))]
  [#:key 'class]
  [#:fold-rhs (match (list pos-flds name-flds methods)
                [(list
                  pos-tys
                  (list (list init-names init-tys reqd) ___)
                  (list (list mname mty) ___))
                 (*Class
                  (map type-rec-id pos-tys)
                  (map list
                       init-names
                       (map type-rec-id init-tys)
                       reqd)
                  (map list mname (map type-rec-id mty)))])])

;; cls : Class
(def-type Instance ([cls Type/c]) [#:key 'instance])

;; sequences
;; includes lists, vectors, etc
;; tys : sequence produces this set of values at each step
(def-type Sequence ([tys (listof Type/c)])
  [#:frees (λ (f) (combine-frees (map f tys)))]
  [#:key #f] [#:fold-rhs (*Sequence (map type-rec-id tys))])

(def-type Future ([t Type/c]) [#:key 'future])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ugly hack - should use units

(define union-maker (box (lambda args (int-err "Union not yet available"))))

(define (set-union-maker! v) (set-box! union-maker v))
(define (get-union-maker) (unbox union-maker))

(provide set-union-maker! get-union-maker)

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

;; type equality
(define/cond-contract (type-equal? s t)
  (Rep? Rep? . -> . boolean?)
  (eq? (Rep-seq s) (Rep-seq t)))

;; inequality - good
(define/cond-contract (type<? s t)
  (Rep? Rep? . -> . boolean?)
  (< (Rep-seq s) (Rep-seq t)))

(define/cond-contract (type-compare s t)
  (Rep? Rep? . -> . (or/c -1 0 1))
  (cond [(type-equal? s t) 0]
        [(type<? s t) 1]
        [else -1]))

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

;; abstract-many : Names Type -> Scope^n
;; where n is the length of names
(define (abstract-many names ty)
  (define (nameTo name count type)
    (let loop ([outer 0] [ty type])
      (define (sb t) (loop outer t))
      (type-case
       (#:Type sb #:Filter (sub-f sb) #:Object (sub-o sb))
       ty
       [#:F name* (if (eq? name name*) (*B (+ count outer)) ty)]
       ;; necessary to avoid infinite loops
       [#:Union elems (*Union (remove-dups (sort (map sb elems) type<?)))]
       ;; functions
       [#:arr dom rng rest drest kws
              (*arr (map sb dom)
                    (sb rng)
                    (if rest (sb rest) #f)
                    (if drest
                        (cons (sb (car drest))
                              (if (eq? (cdr drest) name) (+ count outer) (cdr drest)))
                        #f)
                    (map sb kws))]
       [#:ValuesDots rs dty dbound
              (*ValuesDots (map sb rs)
                           (sb dty)
                           (if (eq? dbound name) (+ count outer) dbound))]
       [#:ListDots dty dbound
                   (*ListDots (sb dty)
                              (if (eq? dbound name) (+ count outer) dbound))]
       [#:Mu (Scope: body) (*Mu (*Scope (loop (add1 outer) body)))]
       [#:PolyDots n body*
                   (let ([body (remove-scopes n body*)])
                     (*PolyDots n (add-scopes n (loop (+ n outer) body))))]
       [#:Poly n body*
               (let ([body (remove-scopes n body*)])
                 (*Poly n (add-scopes n (loop (+ n outer) body))))])))
  (let ([n (length names)])
    (let loop ([ty ty] [names names] [count (sub1 n)])
      (if (zero? count)
          (add-scopes n (nameTo (car names) 0 ty))
          (loop (nameTo (car names) count ty)
                (cdr names)
                (sub1 count))))))

;(trace abstract-many)


;; instantiate-many : List[Type] Scope^n -> Type
;; where n is the length of types
;; all of the types MUST be Fs
(define (instantiate-many images sc)
  (define (replace image count type)
    (let loop ([outer 0] [ty type])
      (define (sb t) (loop outer t))
      (define sf (sub-f sb))
      (type-case
       (#:Type sb #:Filter sf #:Object (sub-o sb))
       ty
       [#:B idx (if (= (+ count outer) idx)
                    image
                    ty)]
       ;; necessary to avoid infinite loops
       [#:Union elems (*Union (remove-dups (sort (map sb elems) type<?)))]
       ;; functions
       [#:arr dom rng rest drest kws
              (*arr (map sb dom)
                    (sb rng)
                    (if rest (sb rest) #f)
                    (if drest
                        (cons (sb (car drest))
                              (if (eqv? (cdr drest) (+ count outer)) (F-n image) (cdr drest)))
                        #f)
                    (map sb kws))]
       [#:ValuesDots rs dty dbound
                     (*ValuesDots (map sb rs)
                                  (sb dty)
                                  (if (eqv? dbound (+ count outer)) (F-n image) dbound))]
       [#:ListDots dty dbound
                   (*ListDots (sb dty)
                              (if (eqv? dbound (+ count outer)) (F-n image) dbound))]
       [#:Mu (Scope: body) (*Mu (*Scope (loop (add1 outer) body)))]
       [#:PolyDots n body*
                   (let ([body (remove-scopes n body*)])
                     (*PolyDots n (add-scopes n (loop (+ n outer) body))))]
       [#:Poly n body*
               (let ([body (remove-scopes n body*)])
                 (*Poly n (add-scopes n (loop (+ n outer) body))))])))
  (let ([n (length images)])
    (let loop ([ty (remove-scopes n sc)] [images images] [count (sub1 n)])
      (if (zero? count)
          (replace (car images) 0 ty)
          (loop (replace (car images) count ty)
                (cdr images)
                (sub1 count))))))

(define (abstract name ty)
  (abstract-many (list name) ty))

(define (instantiate type sc)
  (instantiate-many (list type) sc))

#;(trace instantiate-many abstract-many)

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
(define (Poly* names body)
  (if (null? names) body
      (let ([v (*Poly (length names) (abstract-many names body))])
        (hash-set! name-table v names)
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

;(trace subst subst-all)

(provide
 Mu-name: Poly-names:
 PolyDots-names:
 Type-seq
 Mu-unsafe: Poly-unsafe:
 PolyDots-unsafe:
 Mu? Poly? PolyDots?
 Type? Filter? Object?
 Type/c
 Poly-n
 PolyDots-n
 free-vars*
 type-compare type<?
 remove-dups
 sub-f sub-o sub-pe
 Values: Values? Values-rs
 (rename-out [Mu:* Mu:]
             [Poly:* Poly:]
             [PolyDots:* PolyDots:]
             [Mu* make-Mu]
             [Poly* make-Poly]
             [PolyDots* make-PolyDots]
             [Mu-body* Mu-body]
             [Poly-body* Poly-body]
             [PolyDots-body* PolyDots-body]))

(provide/cond-contract [type-equal? (Rep? Rep? . -> . boolean?)])

;(trace unfold)
