#lang racket/base
(require (except-in "../utils/utils.rkt" infer)
         racket/match racket/function racket/lazy-require racket/list
         (prefix-in c: (contract-req))
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils early-return)
         (types utils resolve base-abbrev match-expanders
                numeric-tower substitute current-seen)
         (for-syntax racket/base syntax/parse unstable/sequence))

(lazy-require
  ("union.rkt" (Un))
  ("../infer/infer.rkt" (infer)))

(define subtype-cache (make-hash))

(define-syntax-rule (handle-failure e)
  e)

;; is s a subtype of t?
;; type type -> boolean
(define/cond-contract (subtype s t)
  (c:-> (c:or/c Type/c SomeValues/c) (c:or/c Type/c SomeValues/c) boolean?)
  (and (subtype* (current-seen) s t) #t))

;; are all the s's subtypes of all the t's?
;; [type] [type] -> boolean
(define (subtypes s t) (and (subtypes* (current-seen) s t) #t))

;; check subtyping for two lists of types
;; List[(cons Number Number)] listof[type] listof[type] -> Opt[List[(cons Number Number)]]
(define (subtypes* A ss ts)
  (cond [(and (null? ss) (null? ts) A)]
        [(or (null? ss) (null? ts)) #f]
        [(subtype* A (car ss) (car ts))
         =>
         (lambda (A*) (subtypes* A* (cdr ss) (cdr ts)))]
        [else #f]))

;; check if s is a supertype of any element of ts
(define (supertype-of-one/arr A s ts)
  (ormap (lambda (e) (arr-subtype*/no-fail A e s)) ts))

(define-syntax (let*/and stx)
  (syntax-parse stx
    [(_ () . e) (syntax/loc stx (let () . e))]
    [(_ ([id expr] . rest) . body)
     (syntax/loc stx
       (let ([id expr])
         (and id (let*/and rest . body))))]))

;; do notation for the subtyping monad
(define-syntax (subtype-seq stx)
  (define-syntax-class sub*
    (pattern e:expr))
  (syntax-parse stx
    [(_ init (s:sub* . args) ...+)
     (with-syntax ([(A* ... A-last) (generate-temporaries #'(s ...))])
       (with-syntax ([(clauses ...)
                      (for/list ([s (in-syntax #'(s ...))]
                                 [args (in-syntax #'(args ...))]
                                 [A (in-syntax #'(init A* ...))]
                                 [A-next (in-syntax #'(A* ... A-last))])
                         #`[#,A-next (#,s #,A . #,args)])])
        (syntax/loc stx (let*/and (clauses ...)
			   A-last))))]))

;; kw-subtypes : (Listof (Pairof Num Num)) (Listof Keyword) (Listof Keyword)
;;               -> (Option (Listof (Pairof Num Num)))
;;
;; Given function types F_s and F_t, this procedure is called to check that the
;; keyword types s-kws for F_s are subtypes of the keyword types t-kws for F_t
;; when checking that F_s <: F_t (but *not* F_t <: F_s).
;;
;; Note that in terms of width, s-kws may have more keywords (i.e., F_s accepts
;; all keywords that F_t does) but the types in s-kws must be supertypes of those
;; in t-kws (i.e., F_s domain types are at least as permissive as those of F_t).
(define (kw-subtypes* A0 s-kws t-kws)
  (let loop ([A A0] [s s-kws] [t t-kws])
    (and
     A
     (match* (s t)
       [((cons (Keyword: ks ts rs) rest-s) (cons (Keyword: kt tt rt) rest-t))
	(cond [(eq? kt ks)
	       (and ;; if t is optional, s must be as well
		(or rt (not rs))
		(loop (subtype* A tt ts) rest-s rest-t))]
	      ;; optional extra keywords in s are ok
	      ;; we just ignore them
	      [(and (not rs) (keyword<? ks kt)) (loop A rest-s t)]
	      ;; extra keywords in t are a problem
	      [else #f])]
       ;; no more keywords to satisfy, the rest in t must be optional
       [(_ '()) (and (andmap (match-lambda [(Keyword: _ _ rs) (not rs)]) s) A)]
       ;; we failed to satisfy all the keyword
       [(_ _) #f]))))

;; simple co/contra-variance for ->
(define (arr-subtype*/no-fail A0 s t)
    (match* (s t)
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
                    (kw-subtypes* s-kws t-kws)
                    (subtype* s2 t2))]
      [((arr: s-dom s-rng s-rest #f s-kws)
        (arr: t-dom t-rng #f #f t-kws))
       (subtype-seq A0
                    (subtypes*/varargs t-dom s-dom s-rest)
                    (kw-subtypes* s-kws t-kws)
                    (subtype* s-rng t-rng))]
      [((arr: s-dom s-rng #f #f s-kws)
        (arr: t-dom t-rng t-rest #f t-kws))
       #f]
      [((arr: s-dom s-rng s-rest #f s-kws)
        (arr: t-dom t-rng t-rest #f t-kws))
       (subtype-seq A0
                    (subtypes*/varargs t-dom s-dom s-rest)
                    (subtype* t-rest s-rest)
                    (kw-subtypes* s-kws t-kws)
                    (subtype* s-rng t-rng))]
      ;; handle ... varargs when the bounds are the same
      [((arr: s-dom s-rng #f (cons s-drest dbound) s-kws)
        (arr: t-dom t-rng #f (cons t-drest dbound) t-kws))
       (subtype-seq A0
                    (subtype* t-drest s-drest)
                    (subtypes* t-dom s-dom)
                    (kw-subtypes* s-kws t-kws)
                    (subtype* s-rng t-rng))]
      [(_ _) #f]))

;; check subtyping of filters, so that predicates subtype correctly
(define (filter-subtype* A0 s t)
  (match* (s t)
   [(f f) A0]
   [((Bot:) t) A0]
   [(s (Top:)) A0]
   [(_ _) #f]))

(define (subtypes/varargs args dom rst)
  (handle-failure (and (subtypes*/varargs null args dom rst) #t)))

(define (subtypes*/varargs A0 argtys dom rst)
  (let loop-varargs ([dom dom] [argtys argtys] [A A0])
    (cond
      [(not A) #f]
      [(and (null? dom) (null? argtys)) A]
      [(null? argtys) #f]
      [(and (null? dom) rst)
       (cond [(subtype* A (car argtys) rst) => (lambda (A) (loop-varargs dom (cdr argtys) A))]
             [else #f])]
      [(null? dom) #f]
      [(subtype* A (car argtys) (car dom)) => (lambda (A) (loop-varargs (cdr dom) (cdr argtys) A))]
      [else #f])))

;(trace subtypes*/varargs)

(define/cond-contract (combine-arrs arrs)
  (c:-> (c:listof arr?) (c:or/c #f arr?))
  (match arrs
    [(list (and a1 (arr: dom1 rng1 #f #f '())) (arr: dom rng #f #f '()) ...)
     (cond
       [(null? dom) (make-arr dom1 rng1 #f #f '())]
       [(not (apply = 1 (length dom1) (map length dom))) #f]
       [(not (for/and ([rng2 (in-list rng)]) (type-equal? rng1 rng2)))
        #f]
       [else (make-arr (apply map Un (cons dom1 dom)) rng1 #f #f '())])]
    [_ #f]))

(define-match-expander NameStruct:
  (lambda (stx)
    (syntax-case stx ()
      [(_ i)
       #'(or (and (Name: _ _ _ #t)
                  (app resolve-once (? Struct? i)))
             (App: (and (Name: _ _ _ #t)
                        (app resolve-once (Poly: _ (? Struct? i))))
                   _ _))])))

(define (subtype/flds* A flds flds*)
  (for/fold ([A A]) ([f (in-list flds)] [f* (in-list flds*)] #:break (not A))
    (and
     A
     (match* (f f*)
       [((fld: t _ #t) (fld: t* _ #t))
	(subtype-seq A
		     (subtype* t* t)
		     (subtype* t t*))]
       [((fld: t _ #f) (fld: t* _ #f))
	(subtype* A t t*)]
       [(_ _) #f]))))

(define (unrelated-structs s1 s2)
  (define (in-hierarchy? s par)
    (define s-name
      (match s
        [(Poly: _ (Struct: s-name _ _ _ _ _)) s-name]
        [(Struct: s-name _ _ _ _ _) s-name]))
    (define p-name
      (match par
        [(Poly: _ (Struct: p-name _ _ _ _ _)) p-name]
        [(Struct: p-name _ _ _ _ _) p-name]))
    (or (free-identifier=? s-name p-name)
        (match s
          [(Poly: _ (? Struct? s*)) (in-hierarchy? s* par)]
          [(Struct: _ (and (Name: _ _ _ #t) p) _ _ _ _)
           (in-hierarchy? (resolve-once p) par)]
          [(Struct: _ (? Struct? p) _ _ _ _) (in-hierarchy? p par)]
          [(Struct: _ (Poly: _ p) _ _ _ _) (in-hierarchy? p par)]
          [(Struct: _ #f _ _ _ _) #f]
          [_ (int-err "wtf is this? ~a" s)])))
  (not (or (in-hierarchy? s1 s2) (in-hierarchy? s2 s1))))

(define/cond-contract (type-equiv? A0 s t)
  (c:-> list? Type? Type? c:any/c)
  (subtype-seq A0
	       (subtype* s t)
	       (subtype* t s)))

(define bottom-key (Rep-seq -Bottom))
(define top-key (Rep-seq Univ))

;; the algorithm for recursive types transcribed directly from TAPL, pg 305
;; List[(cons Number Number)] type type -> List[(cons Number Number)] or #f
;; is s a subtype of t, taking into account previously seen pairs A
(define/cond-contract (subtype* A s t)
  (c:-> (c:listof (c:cons/c fixnum? fixnum?)) Type? Type? c:any/c)
  (define ss (unsafe-Rep-seq s))
  (define st (unsafe-Rep-seq t))
  (early-return
   #:return-when (or (eq? ss st) (seen? ss st A)) A
   (define cr (hash-ref subtype-cache (cons ss st) 'missing))
   #:return-when (boolean? cr) (and cr A)
   (define ks (unsafe-Type-key s))
   (define kt (unsafe-Type-key t))
   #:return-when (and (symbol? ks) (symbol? kt) (not (eq? ks kt))) #f
   #:return-when (and (symbol? ks) (pair? kt) (not (memq ks kt))) #f
   #:return-when 
   (and (pair? ks) (pair? kt)
        (for/and ([i (in-list ks)]) (not (memq i kt))))
   #f
   #:return-when (eq? ss bottom-key) A
   #:return-when (eq? st top-key) A
   (define A0 (remember s t A))
   (define r
     ;; FIXME -- make this go into only the places that need it -- slows down new-metrics.rkt significantly
     (update-current-seen A0
       (match* (s t)
         ;; these cases are above as special cases
         ;; [((Union: (list)) _) A0] ;; this is extremely common, so it goes first
         ;; [(_ (Univ:)) A0]
         ;; error is top and bot
         [(_ (Error:)) A0]
         [((Error:) _) A0]
         ;; (Un) is bot
         [(_ (Union: (list))) #f]
         ;; value types
         [((Value: v1) (Value: v2))
          #:when (equal? v1 v2) A0]
         ;; values are subtypes of their "type"
         [((Value: v) (Base: _ _ pred _)) (if (pred v) A0 #f)]
         ;; tvars are equal if they are the same variable
         [((F: t) (F: t*)) (if (eq? t t*) A0 #f)]
         ;; Avoid needing to resolve things that refer to different structs.
         ;; Saves us from non-termination
         ;; Must happen *before* the sequence cases, which sometimes call `resolve' in match expanders
         [((or (? Struct? s1) (NameStruct: s1)) (or (? Struct? s2) (NameStruct: s2)))
          #:when (unrelated-structs s1 s2)
          #f]
         ;; similar case for structs and base types, which are obviously unrelated
         [((Base: _ _ _ _) (or (? Struct? s1) (NameStruct: s1)))
          #f]
         [((or (? Struct? s1) (NameStruct: s1)) (Base: _ _ _ _))
          #f]
         ;; same for all values.
         [((Value: (? (negate struct?) _)) (or (? Struct? s1) (NameStruct: s1)))
          #f]
         [((or (? Struct? s1) (NameStruct: s1)) (Value: (? (negate struct?) _)))
          #f]
         ;; sequences are covariant
         [((Sequence: ts) (Sequence: ts*))
          (subtypes* A0 ts ts*)]
         [((Listof: t) (Sequence: (list t*)))
          (subtype* A0 t t*)]
         [((Pair: t1 t2) (Sequence: (list t*)))
          (subtype-seq A0 (subtype* t1 t*) (subtype* t2 (-lst t*)))]
         [((MListof: t) (Sequence: (list t*)))
          (subtype* A0 t t*)]
         ;; To check that mutable pair is a sequence we check that the cdr
         ;; is both an mutable list and a sequence
         [((MPair: t1 t2) (Sequence: (list t*)))
          (subtype-seq A0
                       (subtype* t1 t*)
                       (subtype* t2 (simple-Un -Null (make-MPairTop)))
                       (subtype* t2 t))]
         ;; Note: this next case previously used the List: match expander, but
         ;;       using that would cause an infinite loop in certain cases
         ;;       (i.e., Struct types, see PR 14364) because the expander
         ;;       uses `resolve`. This is not normally a problem, but during
         ;;       subtyping it's dangerous to call functions that can cause
         ;;       substitution and thus more subtyping checks.
         ;;
         ;;       Instead, we can just check for Null here since combined with
         ;;       the Pair: case above and resolution of types like Mu, all the
         ;;       List: cases should be covered.
         [((Value: '()) (Sequence: (list t*))) A0]
         [((HeterogeneousVector: ts) (Sequence: (list t*)))
          (subtypes* A0 ts (map (λ (_) t*) ts))]
         [((Vector: t) (Sequence: (list t*)))
          (subtype* A0 t t*)]
         [((Base: 'FlVector _ _ _) (Sequence: (list t*)))
          (subtype* A0 -Flonum t*)]
         [((Base: 'ExtFlVector _ _ _) (Sequence: (list t*)))
          (subtype* A0 -ExtFlonum t*)]
         [((Base: 'FxVector _ _ _) (Sequence: (list t*)))
          (subtype* A0 -Fixnum t*)]
         [((Base: 'String _ _ _) (Sequence: (list t*)))
          (subtype* A0 -Char t*)]
         [((Base: 'Bytes _ _ _) (Sequence: (list t*)))
          (subtype* A0 -Byte t*)]
         [((Base: 'Input-Port _ _ _) (Sequence: (list t*)))
          (subtype* A0 -Nat t*)]
         [((Value: (? exact-nonnegative-integer? n)) (Sequence: (list t*)))
          (define possibilities
            (list
             (list byte? -Byte)
             (list portable-index? -Index)
             (list portable-fixnum? -NonNegFixnum)
             (list values -Nat)))
          (define type
            (for/or ((pred-type (in-list possibilities)))
              (match pred-type
                ((list pred? type)
                 (and (pred? n) type)))))
          (subtype* A0 type t*)]
         [((Base: _ _ _ #t) (Sequence: (list t*)))
          (define type
            ;; FIXME: thread the store through here
            (for/or ((t (in-list (list -Byte -Index -NonNegFixnum -Nat))))
              (or (and (subtype* A0 s t) t))))
          (if type
              (subtype* A0 type t*)
              #f)]
         [((Hashtable: k v) (Sequence: (list k* v*)))
          (subtypes* A0 (list k v) (list k* v*))]
         [((Set: t) (Sequence: (list t*)))
          (subtype* A0 t t*)]
         ;; special-case for case-lambda/union with only one argument              
         [((Function: arr1) (Function: (list arr2)))
          (cond [(null? arr1) #f]
                [else
                 (define comb (combine-arrs arr1))
                 (or (and comb (arr-subtype*/no-fail A0 comb arr2))
                     (supertype-of-one/arr A0 arr2 arr1))])]
         ;; case-lambda
         [((Function: arr1) (Function: arr2))
          (if (null? arr1) #f
              (let loop-arities ([A* A0]
                                 [arr2 arr2])
                (cond
                  [(null? arr2) A*]
                  [(supertype-of-one/arr A* (car arr2) arr1) => (lambda (A) (loop-arities A (cdr arr2)))]
                  [else #f])))]
         ;; recur structurally on pairs
         [((Pair: a d) (Pair: a* d*))
          (subtypes* A0 (list a d) (list a* d*))]
         ;; recur structurally on dotted lists, assuming same bounds
         [((ListDots: s-dty dbound) (ListDots: t-dty dbound*))
          (and (eq? dbound dbound*)
               (subtype* A0 s-dty t-dty))]
         ;; For dotted lists and regular lists, we check that (All (dbound) s-dty) is a subtype
         ;; of t-elem, so that no matter what dbound is instatiated with s-dty is still a subtype
         ;; of t-elem. We cannot just replace dbound with Univ because of variance issues.
         [((ListDots: s-dty dbound) (Listof: t-elem))
          (subtype* A0 (-poly (dbound) s-dty) t-elem)]
         ;; quantification over two types preserves subtyping
         [((Poly: ns b1) (Poly: ms b2))               
          #:when (= (length ns) (length ms))
          ;; substitute ns for ms in b2 to make it look like b1
          (subtype* A0 b1 (subst-all (make-simple-substitution ms (map make-F ns)) b2))]
         [((PolyDots: (list ns ... n-dotted) b1)
           (PolyDots: (list ms ... m-dotted) b2))
          (cond
            [(< (length ns) (length ms))
             (define-values (short-ms rest-ms) (split-at ms (length ns)))
             ;; substitute ms for ns in b1 to make it look like b2
             (define subst
               (hash-set (make-simple-substitution ns (map make-F short-ms))
                         n-dotted (i-subst/dotted (map make-F rest-ms) (make-F m-dotted) m-dotted)))
             (subtype* A0 (subst-all subst b1) b2)]
            [else
             (define-values (short-ns rest-ns) (split-at ns (length ms)))
             ;; substitute ns for ms in b2 to make it look like b1
             (define subst
               (hash-set (make-simple-substitution ms (map make-F short-ns))
                         m-dotted (i-subst/dotted (map make-F rest-ns) (make-F n-dotted) n-dotted)))
             (subtype* A0 b1 (subst-all subst b2))])]
         [((PolyDots: (list ns ... n-dotted) b1)
           (Poly: (list ms ...) b2))
          #:when (<= (length ns) (length ms))
          ;; substitute ms for ns in b1 to make it look like b2
          (define subst
            (hash-set (make-simple-substitution ns (map make-F (take ms (length ns))))
                      n-dotted (i-subst (map make-F (drop ms (length ns))))))
          (subtype* A0 (subst-all subst b1) b2)]
         [((Refinement: par _) t)
          (subtype* A0 par t)]
         ;; use unification to see if we can use the polytype here
         [((Poly: vs b) s)
          #:when (infer vs null (list b) (list s) Univ)
          A0]
         [((PolyDots: (list vs ... vdotted) b) s)
          #:when (infer vs (list vdotted) (list b) (list s) Univ)
          A0]
         [(s (or (Poly: vs b) (PolyDots: vs b)))
          #:when (null? (fv b))
          (subtype* A0 s b)]
         ;; rec types, applications and names (that aren't the same)
         [((? needs-resolving? s) other)
          (let ([s* (resolve-once s)])
            (if (Type/c? s*) ;; needed in case this was a name that hasn't been resolved yet
                (subtype* A0 s* other)
                #f))]
         [(other (? needs-resolving? t))
          (let ([t* (resolve-once t)])
            (if (Type/c? t*) ;; needed in case this was a name that hasn't been resolved yet
                (subtype* A0 other t*)
                #f))]
         ;; for unions, we check the cross-product
         ;; some special cases for better performance
         ;; first, if both types are numeric, they will be built from the same base types
         ;; so we can check for simple set inclusion of the union components
         [((Base: _ _ _ #t) (Union: l2))
          #:when (eq? kt 'number)
          (and (memq s l2) A0)]
         [((Union: l1) (Union: l2))
          #:when (and (eq? ks 'number) (eq? kt 'number))
          ;; l1 should be a subset of l2
          ;; since union elements are sorted, a linear scan works
          (let loop ([l1 l1] [l2 l2])
            (cond [(null? l1)
                   A0]
                  [(null? l2)
                   #f]
                  [(eq? (car l1) (car l2))
                   (loop (cdr l1) (cdr l2))]
                  [else
                   (loop l1 (cdr l2))]))]
         [((Union: es) t)
          (and 
           (for/and ([elem (in-list es)])
             (subtype* A0 elem t))
           A0)]
         [(s (Union: es))
          (and (for/or ([elem (in-list es)])
                 (subtype* A0 s elem))
               A0)]
         ;; subtyping on immutable structs is covariant
         [((Struct: nm _ flds proc _ _) (Struct: nm* _ flds* proc* _ _)) 
          #:when (free-identifier=? nm nm*) 
          (let ([A (cond [(and proc proc*) (subtype* A0 proc proc*)]
                         [proc* #f]
                         [else A0])])
            (and A (subtype/flds* A flds flds*)))]
         [((Struct: nm _ _ _ _ _) (StructTop: (Struct: nm* _ _ _ _ _)))
          #:when (free-identifier=? nm nm*)
          A0]
         ;; All struct-type types are subtypes of the struct type top type
         [((StructType: _) (StructTypeTop:)) A0]
         ;; Promises are covariant
         [((Promise: s) (Promise: t))
          (subtype* A0 s t)]
         ;ephemerons are covariant
         [((Ephemeron: s) (Ephemeron: t))
          (subtype* A0 s t)]
         [((CustodianBox: s) (CustodianBox: t))
          (subtype* A0 s t)]
         [((Set: t) (Set: t*)) (subtype* A0 t t*)]
         ;; Evts are covariant
         [((Evt: t) (Evt: t*)) (subtype* A0 t t*)]
         [((Base: 'Semaphore _ _ _) (Evt: t))
          (subtype* A0 s t)]
         [((Base: 'Output-Port _ _ _) (Evt: t))
          (subtype* A0 s t)]
         [((Base: 'Input-Port _ _ _) (Evt: t))
          (subtype* A0 s t)]
         [((Base: 'TCP-Listener _ _ _) (Evt: t))
          (subtype* A0 s t)]
         [((Base: 'Thread _ _ _) (Evt: t))
          (subtype* A0 s t)]
         [((Base: 'Subprocess _ _ _) (Evt: t))
          (subtype* A0 s t)]
         [((Base: 'Will-Executor _ _ _) (Evt: t))
          (subtype* A0 s t)]
         [((Base: 'LogReceiver _ _ _) (Evt: t))
          (subtype* A0
                    (make-HeterogeneousVector
                     (list -Symbol -String Univ
                           (Un (-val #f) -Symbol)))
                    t)]
         [((CustodianBox: t) (Evt: t*))
          ;; Note that it's the whole box type that's being
          ;; compared against t* here
          (subtype* A0 s t*)]
         [((Channel: t) (Evt: t*)) (subtype* A0 t t*)]
         [((Async-Channel: t) (Evt: t*)) (subtype* A0 t t*)]
         ;; Invariant types
         [((Box: s) (Box: t)) (type-equiv? A0 s t)]
         [((Box: _) (BoxTop:)) A0]
         [((ThreadCell: s) (ThreadCell: t)) (type-equiv? A0 s t)]
         [((ThreadCell: _) (ThreadCellTop:)) A0]
         [((Channel: s) (Channel: t)) (type-equiv? A0 s t)]
         [((Channel: _) (ChannelTop:)) A0]
         [((Async-Channel: s) (Async-Channel: t)) (type-equiv? A0 s t)]
         [((Async-Channel: _) (Async-ChannelTop:)) A0]
         [((Vector: s) (Vector: t)) (type-equiv? A0 s t)]
         [((Vector: _) (VectorTop:)) A0]
         [((HeterogeneousVector: _) (VectorTop:)) A0]
         [((HeterogeneousVector: (list e ...)) (Vector: e*))
          (for/fold ((A A0)) ((e (in-list e)) #:break (not A))
            (and A (type-equiv? A e e*)))]
         [((HeterogeneousVector: (list s* ...)) (HeterogeneousVector: (list t* ...)))
          (if (= (length s*) (length t*))
              (for/fold ((A A0)) ((s (in-list s*)) (t (in-list t*)) #:break (not A))
                (type-equiv? A s t))
              #f)]
         [((MPair: s1 s2) (MPair: t1 t2))
          (subtype-seq A0
                       (type-equiv? s1 t1)
                       (type-equiv? s2 t2))]
         [((MPair: _ _) (MPairTop:)) A0]
         [((Hashtable: s1 s2) (Hashtable: t1 t2))
          (subtype-seq A0
                       (type-equiv? s1 t1)
                       (type-equiv? s2 t2))]
         [((Hashtable: _ _) (HashtableTop:)) A0]
         [((Prompt-Tagof: s1 s2) (Prompt-Tagof: t1 t2))
          (subtype-seq A0
                       (type-equiv? s1 t1)
                       (type-equiv? s2 t2))]
         [((Prompt-Tagof: _ _) (Prompt-TagTop:)) A0]
         [((Continuation-Mark-Keyof: s) (Continuation-Mark-Keyof: t))
          (type-equiv? A0 s t)]
         [((Continuation-Mark-Keyof: _) (Continuation-Mark-KeyTop:)) A0]
         ;; subtyping on structs follows the declared hierarchy
         [((Struct: nm (? Type/c? parent) _ _ _ _) other)
          (subtype* A0 parent other)]
         ;; subtyping on values is pointwise, except special case for Bottom
         [((Values: (list (Result: (== -Bottom) _ _))) _)
          A0]
         [((Values: vals1) (Values: vals2)) (subtypes* A0 vals1 vals2)]
         [((ValuesDots: s-rs s-dty dbound) (ValuesDots: t-rs t-dty dbound))
          (subtype-seq A0
                       (subtypes* s-rs t-rs)
                       (subtype* s-dty t-dty))]
         [((AnyValues: s-f) (AnyValues: t-f))
          (filter-subtype* A0 s-f t-f)]
         [((or (Values: (list (Result: _ fs _) ...))
               (ValuesDots: (list (Result: _ fs _) ...) _ _))
           (AnyValues: t-f))
          (for/or ([f (in-list fs)])
            (match f
              [(FilterSet: f+ f-)
               (subtype-seq A0
                            (filter-subtype* f+ t-f)
                            (filter-subtype* f- t-f))]))]
         [((Result: t (FilterSet: ft ff) o) (Result: t* (FilterSet: ft* ff*) o))
          (subtype-seq A0
                       (subtype* t t*)
                       (filter-subtype* ft ft*)
                       (filter-subtype* ff ff*))]
         [((Result: t (FilterSet: ft ff) o) (Result: t* (FilterSet: ft* ff*) (Empty:)))
          (subtype-seq A0
                       (subtype* t t*)
                       (filter-subtype* ft ft*)
                       (filter-subtype* ff ff*))]
         ;; subtyping on other stuff
         [((Syntax: t) (Syntax: t*))
          (subtype* A0 t t*)]
         [((Future: t) (Future: t*))
          (subtype* A0 t t*)]
         [((Param: s-in s-out) (Param: t-in t-out))
          (subtype-seq A0
                       (subtype* t-in s-in)
                       (subtype* s-out t-out))]
         [((Param: in out) t)
          (subtype* A0 (cl->* (-> out) (-> in -Void)) t)]
         [((Instance: (? needs-resolving? s)) other)
          (let ([s* (resolve-once s)])
            (if (Type/c? s*)
                (subtype* A0 (make-Instance s*) other)
                #f))]
         [(other (Instance: (? needs-resolving? t)))
          (let ([t* (resolve-once t)])
            (if (Type/c? t*)
                (subtype* A0 other (make-Instance t*))
                #f))]
         [((Instance: (Class: _ _ field-map method-map augment-map _))
           (Instance: (Class: _ _ field-map* method-map* augment-map* _)))
          (define (subtype-clause? map map*)
            (and (for/and ([key+type (in-list map*)])
                   (match-define (list key type) key+type)
                   (assq key map))
                 (let/ec escape
                   (for/fold ([A A0])
                             ([key+type (in-list map)])
                     (match-define (list key type) key+type)
                     (define result (assq (car key+type) map*))
                     (or (and (not result) A)
                         (let ([type* (cadr result)])
                           (or (subtype* A type type*)
                               (escape #f))))))))
          (and ;; Note that init & augment clauses don't matter for objects
               (subtype-clause? method-map method-map*)
               (subtype-clause? field-map field-map*))]
         [((? Class?) (ClassTop:)) A0]
         [((Class: row inits fields methods augments init-rest)
           (Class: row* inits* fields* methods* augments* init-rest*))
          ;; TODO: should the result be folded instead?
          (define sub (curry subtype* A))
          ;; check that each of inits, fields, methods, etc. are
          ;; equal by sorting and checking type equality
          (define (equal-clause? clause clause* [inits? #f])
            (cond
             [(not inits?)
              (match-define (list (list names types) ...) clause)
              (match-define (list (list names* types*) ...) clause*)
              (and (= (length names) (length names*))
                   (andmap equal? names names*)
                   (andmap sub types types*))]
             [else
              (match-define (list (list names types opt?) ...)
                            clause)
              (match-define (list (list names* types* opt?*) ...)
                            clause*)
              (and (= (length names) (length names*))
                   (andmap equal? names names*)
                   (andmap sub types types*)
                   (andmap equal? opt? opt?*))]))
          ;; There is no non-trivial width subtyping on class types, but it's
          ;; possible for two "equal" class types to look different
          ;; in the representation. We deal with that here.
          (and (or (and (or (Row? row) (not row))
                        (or (Row? row*) (not row*)))
                   (equal? row row*))
               (equal-clause? inits inits* #t)
               (equal-clause? fields fields*)
               (equal-clause? methods methods*)
               (equal-clause? augments augments*)
               (sub init-rest init-rest*))]
         ;; otherwise, not a subtype
         [(_ _) #f])))
     (when (null? A)
       (hash-set! subtype-cache (cons ss st) r))
     r))

(define (type-compare? a b)
  (or (type-equal? a b)
      (and (subtype a b) (subtype b a))))

;; List[(cons Number Number)] type type -> maybe[List[(cons Number Number)]]
(define subtype*/no-fail subtype*)

(provide/cond-contract
 [subtype (c:-> (c:or/c Type/c SomeValues/c) (c:or/c Type/c SomeValues/c) boolean?)]
 [type-compare? (c:-> (c:or/c Type/c SomeValues/c) (c:or/c Type/c SomeValues/c) boolean?)]
 [subtypes (c:-> (c:listof (c:or/c Type/c SomeValues/c))
                 (c:listof (c:or/c Type/c SomeValues/c))
                 boolean?)]
 [subtypes/varargs (c:-> (c:listof Type/c) (c:listof Type/c) (c:or/c Type/c #f) boolean?)])

;(require racket/trace)
;(trace subtype*)
;(trace supertype-of-one/arr)
;(trace arr-subtype*/no-fail)
;(trace subtype*/no-fail)
;(trace subtypes*)
;(trace subtype)

;(subtype (-> Univ B) (-> Univ Univ))
;(subtype (make-poly '(a) (make-tvar 'a)) (make-lst N))

;;problem:
;; (subtype (make-Mu 'x (make-Syntax (make-Union (list (make-Base 'Number #'number? number?) (make-F 'x))))) (make-Syntax (make-Univ)))
