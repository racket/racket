#lang racket/base
(require (except-in "../utils/utils.rkt" infer) racket/unsafe/ops
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)
         (types utils resolve base-abbrev numeric-tower substitute)
         (env type-name-env)
         racket/match unstable/match
         racket/function
         racket/lazy-require
         (prefix-in c: racket/contract)
         (for-syntax racket/base syntax/parse))

(lazy-require
  ("union.rkt" (Un))
  ("../infer/infer.rkt" (infer)))


;; exn representing failure of subtyping
;; s,t both types
(define-struct (exn:subtype exn:fail) (s t))

;; subtyping failure - masked before it gets to the user program
(define-syntax fail!
  (syntax-rules ()
    [(_ s t) (raise (make-exn:subtype "subtyping failed" (current-continuation-marks) s t))]))

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

(define-syntax-rule (handle-failure e)
  (with-handlers ([exn:subtype? (λ (_) #f)])
    e))

;; is s a subtype of t?
;; type type -> boolean
(define/cond-contract (subtype s t)
  (c:-> (c:or/c Type/c Values?) (c:or/c Type/c Values?) boolean?)
  (define k (cons (unsafe-struct-ref s 0) (unsafe-struct-ref t 0)))
  (define (new-val) 
    (define result (handle-failure (and (subtype* (current-seen) s t) #t)))
    ;(printf "subtype cache miss ~a ~a\n" s t)
    result)
  (hash-ref! subtype-cache k new-val))

;; are all the s's subtypes of all the t's?
;; [type] [type] -> boolean
(define (subtypes s t) (handle-failure (subtypes* (current-seen) s t)))

;; subtyping under constraint set, but produces boolean result instead of raising exn
;; List[(cons Number Number)] type type -> maybe[List[(cons Number Number)]]
(define (subtype*/no-fail A s t) (handle-failure (subtype* A s t)))

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

(define-syntax (subtype-seq stx)
  (define-syntax-class sub*
    (pattern e:expr))
  (syntax-parse stx
    [(_ init (s1:sub* . args1) (s:sub* . args) ...)
     (with-syntax ([(A* ... A-last) (generate-temporaries #'(s1 s ...))])
       (with-syntax ([(clauses ...)
                      (for/list ([s (syntax->list #'(s1 s ...))]
                                 [args (syntax->list #'(args1 args ...))]
                                 [A (syntax->list #'(init A* ...))]
                                 [A-next (syntax->list #'(A* ... A-last))])
                         #`[#,A-next (#,s #,A . #,args)])])
        #'(let* (clauses ...)
            A-last)))]))

(define (kw-subtypes* A0 t-kws s-kws)
  (let loop ([A A0] [t t-kws] [s s-kws])
    (match* (t s)
      [((list (Keyword: kt tt rt) rest-t) (list (Keyword: ks ts rs) rest-s))
       (cond [(eq? kt ks)
              (if
               ;; if s is optional, t must be as well
               (or rs (not rt))
               (loop (subtype* A tt ts) rest-t rest-s)
               (fail! t s))]
             ;; extra keywords in t are ok
             ;; we just ignore them
             [(keyword<? kt ks) (loop A rest-t s)]
             ;; extra keywords in s are a problem
             [else (fail! t s)])]
      ;; no more keywords to satisfy
      [(_ '()) A]
      ;; we failed to satisfy all the keyword
      [(_ _) (fail! s t)])))

;; simple co/contra-variance for ->
(define (arr-subtype*/no-fail A0 s t)
  (with-handlers
      ([exn:subtype? (lambda _ #f)])
    (match* (s t)
      ;; top for functions is above everything
      [(_ (top-arr:)) A0]
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
                    (kw-subtypes* t-kws s-kws)
                    (subtype* s2 t2))]
      [((arr: s-dom s-rng s-rest #f s-kws)
        (arr: t-dom t-rng #f #f t-kws))
       (subtype-seq A0
                    (subtypes*/varargs t-dom s-dom s-rest)
                    (kw-subtypes* t-kws s-kws)
                    (subtype* s-rng t-rng))]
      [((arr: s-dom s-rng #f #f s-kws)
        (arr: t-dom t-rng t-rest #f t-kws))
       (fail! s t)]
      [((arr: s-dom s-rng s-rest #f s-kws)
        (arr: t-dom t-rng t-rest #f t-kws))
       (subtype-seq A0
                    (subtypes*/varargs t-dom s-dom s-rest)
                    (subtype* t-rest s-rest)
                    (kw-subtypes* t-kws s-kws)
                    (subtype* s-rng t-rng))]
      ;; handle ... varargs when the bounds are the same
      [((arr: s-dom s-rng #f (cons s-drest dbound) s-kws)
        (arr: t-dom t-rng #f (cons t-drest dbound) t-kws))
       (subtype-seq A0
                    (subtype* t-drest s-drest)
                    (subtypes* t-dom s-dom)
                    (kw-subtypes* t-kws s-kws)
                    (subtype* s-rng t-rng))]
      [(_ _)
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
       #'(or (and (Name: _) (app resolve-once (? Struct? i)))
             (App: (and (Name: _) (app resolve-once (Poly: _ (? Struct? i)))) _ _))])))

(define (subtype/flds* A flds flds*)
  (for/fold ([A A]) ([f (in-list flds)] [f* (in-list flds*)])
    (match* (f f*)
      [((fld: t _ #t) (fld: t* _ #t))
       (subtype* (subtype* A t* t) t t*)]
      [((fld: t _ #f) (fld: t* _ #f))
       (subtype* A t t*)])))

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
          [(Struct: _ (and (Name: _) p) _ _ _ _) (in-hierarchy? (resolve-once p) par)]
          [(Struct: _ (? Struct? p) _ _ _ _) (in-hierarchy? p par)]
          [(Struct: _ (Poly: _ p) _ _ _ _) (in-hierarchy? p par)]
          [(Struct: _ #f _ _ _ _) #f]
          [_ (int-err "wtf is this? ~a" s)])))
  (not (or (in-hierarchy? s1 s2) (in-hierarchy? s2 s1))))

;; the algorithm for recursive types transcribed directly from TAPL, pg 305
;; List[(cons Number Number)] type type -> List[(cons Number Number)]
;; potentially raises exn:subtype, when the algorithm fails
;; is s a subtype of t, taking into account constraints A
(define (subtype* A s t)
  (define =t (lambda (a b) (if (and (Rep? a) (Rep? b)) (type-equal? a b) (equal? a b))))
  (parameterize ([match-equality-test =t]
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
            (match* (s t)
              [(_ (Univ:)) A0]
              ;; error is top and bot
              [(_ (Error:)) A0]
              [((Error:) _) A0]
              ;; (Un) is bot
              [(_ (Union: (list))) (fail! s t)]
              [((Union: (list)) _) A0]
              ;; value types
              [((Value: v1) (Value: v2)) (=> unmatch) (if (equal? v1 v2) A0 (unmatch))]
              ;; values are subtypes of their "type"
              [((Value: v) (Base: _ _ pred _ _)) (if (pred v) A0 (fail! s t))]
              ;; tvars are equal if they are the same variable
              [((F: t) (F: t*)) (if (eq? t t*) A0 (fail! s t))]
              ;; Avoid needing to resolve things that refer to different structs.
              ;; Saves us from non-termination
              ;; Must happen *before* the sequence cases, which sometimes call `resolve' in match expanders
              [((or (? Struct? s1) (NameStruct: s1)) (or (? Struct? s2) (NameStruct: s2)))
               (=> unmatch)
               (cond [(unrelated-structs s1 s2)
                      ;(dprintf "found unrelated structs: ~a ~a\n" s1 s2)
                      (fail! s t)]
                     [else (unmatch)])]
              ;; similar case for structs and base types, which are obviously unrelated
              [((Base: _ _ _ _ _) (or (? Struct? s1) (NameStruct: s1)))
               (fail! s t)]
              [((or (? Struct? s1) (NameStruct: s1)) (Base: _ _ _ _ _))
               (fail! s t)]
              ;; same for all values.
              [((Value: (? (negate struct?) _)) (or (? Struct? s1) (NameStruct: s1)))
               (fail! s t)]
              [((or (? Struct? s1) (NameStruct: s1)) (Value: (? (negate struct?) _)))
               (fail! s t)]
              ;; just checking if s/t is a struct misses recursive/union/etc cases
              [((? (lambda (_) (eq? ks 'struct))) (Base: _ _ _ _ _)) (fail! s t)]
              [((Base: _ _ _ _ _) (? (lambda (_) (eq? kt 'struct)))) (fail! s t)]
              ;; sequences are covariant
              [((Sequence: ts) (Sequence: ts*))
               (subtypes* A0 ts ts*)]
              [((Listof: t) (Sequence: (list t*)))
               (subtype* A0 t t*)]
              [((List: ts) (Sequence: (list t*)))
               (subtypes* A0 ts (map (λ _ t*) ts))]
              [((HeterogeneousVector: ts) (Sequence: (list t*)))
               (subtypes* A0 ts (map (λ _ t*) ts))]
              [((Vector: t) (Sequence: (list t*)))
               (subtype* A0 t t*)]
              [((Base: 'String _ _ _ _) (Sequence: (list t*)))
               (subtype* A0 -Char t*)]
              [((Base: 'Bytes _ _ _ _) (Sequence: (list t*)))
               (subtype* A0 -Byte t*)]
              [((Base: 'Input-Port _ _ _ _) (Sequence: (list t*)))
               (subtype* A0 -Nat t*)]
              [((Hashtable: k v) (Sequence: (list k* v*)))
               (subtypes* A0 (list k v) (list k* v*))]
              ;; special-case for case-lambda/union with only one argument              
              [((Function: arr1) (Function: (list arr2)))
               (when (null? arr1) (fail! s t))
               (define comb (combine-arrs arr1))
               (or (and comb (arr-subtype*/no-fail A0 comb arr2))
                   (supertype-of-one/arr A0 arr2 arr1)
                   (fail! s t))]
              ;; case-lambda
              [((Function: arr1) (Function: arr2))
               (when (null? arr1) (fail! s t))
               (let loop-arities ([A* A0]
                                  [arr2 arr2])
                 (cond
                  [(null? arr2) A*]
                  [(supertype-of-one/arr A* (car arr2) arr1) => (lambda (A) (loop-arities A (cdr arr2)))]
                  [else (fail! s t)]))]
              ;; recur structurally on pairs
              [((Pair: a d) (Pair: a* d*))
               (let ([A1 (subtype* A0 a a*)])
                 (and A1 (subtype* A1 d d*)))]
              ;; recur structurally on dotted lists, assuming same bounds
              [((ListDots: s-dty dbound) (ListDots: t-dty dbound))
               (subtype* A0 s-dty t-dty)]
              [((ListDots: s-dty dbound) (Listof: t-elem))
               (subtype* A0 (substitute Univ dbound s-dty) t-elem)]
              ;; quantification over two types preserves subtyping
              [((Poly: ns b1) (Poly: ms b2))
               (=> unmatch)
               (unless (= (length ns) (length ms))
                       (unmatch))
               (subtype* A0 b1 (subst-all (make-simple-substitution ms (map make-F ns)) b2))]
              [((Refinement: par _ _) t)
               (subtype* A0 par t)]
              ;; use unification to see if we can use the polytype here
              [((Poly: vs b) s)
               (=> unmatch)
               (if (infer vs null (list b) (list s) (make-Univ)) A0 (unmatch))]
              [(s (Poly: vs b))
               (=> unmatch)
               (if (null? (fv b)) (subtype* A0 s b) (unmatch))]
              ;; rec types, applications and names (that aren't the same)
              [((? needs-resolving? s) other)
               (let ([s* (resolve-once s)])
                 (if (Type? s*) ;; needed in case this was a name that hasn't been resolved yet
                     (subtype* A0 s* other)
                     (fail! s t)))]
              [(other (? needs-resolving? t))
               (let ([t* (resolve-once t)])
                 (if (Type? t*) ;; needed in case this was a name that hasn't been resolved yet
                     (subtype* A0 other t*)
                     (fail! s t)))]
              ;; for unions, we check the cross-product
              ;; some special cases for better performance
              ;; first, if both types are numeric, they will be built from the same base types
              ;; so we can check for simple set inclusion of the union components
              [((Base: _ _ _ _ _) (Union: l2))
               (=> unmatch)
               (if (and (eq? ks 'number) (eq? kt 'number))
                   (if (memq s l2) A0 (fail! s t))
                   (unmatch))]
              [((Union: l1) (Union: l2))
               (=> unmatch)
               (if (and (eq? ks 'number) (eq? kt 'number))
                   ;; l1 should be a subset of l2
                   ;; since union elements are sorted, a linear scan works
                   (let loop ([l1 l1] [l2 l2])
                     (cond [(null? l1)
                            A0]
                           [(null? l2)
                            (fail! s t)]
                           [(eq? (car l1) (car l2))
                            (loop (cdr l1) (cdr l2))]
                           [else
                            (loop l1 (cdr l2))]))
                   (unmatch))]
              [((Union: (list e1 e2)) t)
               (if (and (subtype* A0 e1 t) (subtype* A0 e2 t))
                   A0
                   (fail! s t))]
              [((Union: (list e1 e2 e3)) t)
               (if (and (subtype* A0 e1 t) (subtype* A0 e2 t) (subtype* A0 e3 t))
                   A0
                   (fail! s t))]
              [((Union: es) t)
               (if (for/and ([elem (in-list es)])
                     (subtype* A0 elem t))
                   A0
                   (fail! s t))]
              [(s (Union: es))
               (if (for/or ([elem (in-list es)])
                     (with-handlers ([exn:subtype? (lambda _ #f)])
                       (subtype* A0 s elem)))
                   A0
                   (fail! s t))]
              ;; subtyping on immutable structs is covariant
              [((Struct: nm _ flds proc _ _) (Struct: nm* _ flds* proc* _ _)) (=> nevermind)
               (unless (free-identifier=? nm nm*) (nevermind))
               (let ([A (cond [(and proc proc*) (subtype* proc proc*)]
                              [proc* (fail! proc proc*)]
                              [else A0])])
                 (subtype/flds* A flds flds*))]
              [((Struct: nm _ _ _ _ _) (StructTop: (Struct: nm* _ _ _ _ _))) (=> nevermind)
               (unless (free-identifier=? nm nm*) (nevermind))
               A0]
              ;; Promises are covariant
              [((Promise: s) (Promise: t))
               (subtype* A0 s t)]
              ;ephemerons are covariant
              [((Ephemeron: s) (Ephemeron: t))
               (subtype* A0 s t)]
              [((CustodianBox: s) (CustodianBox: t))
               (subtype* A0 s t)]
              [((Box: _) (BoxTop:)) A0]
              [((ThreadCell: _) (ThreadCellTop:)) A0]
              [((Set: t) (Set: t*)) (subtype* A0 t t*)]
              [((Channel: _) (ChannelTop:)) A0]
              [((Vector: _) (VectorTop:)) A0]
              [((HeterogeneousVector: _) (VectorTop:)) A0]
              [((HeterogeneousVector: (list e ...)) (Vector: e*))
               (if (andmap (lambda (e0) (type-equal? e0 e*)) e) A0 (fail! s t))]
              [((MPair: _ _) (MPairTop:)) A0]
              [((Hashtable: _ _) (HashtableTop:)) A0]
              ;; TODO: subtyping for two `Prompt-Tagof`s with recursive types
              ;;       may be rejected unnecessarily
              [((Prompt-Tagof: _ _) (Prompt-TagTop:)) A0]
              [((Continuation-Mark-Keyof: _) (Continuation-Mark-KeyTop:)) A0]
              ;; subtyping on structs follows the declared hierarchy
              [((Struct: nm (? Type? parent) _ _ _ _) other)
               ;(dprintf "subtype - hierarchy : ~a ~a ~a\n" nm parent other)
               (subtype* A0 parent other)]
              ;; subtyping on values is pointwise
              [((Values: vals1) (Values: vals2)) (subtypes* A0 vals1 vals2)]
              ;; trivial case for Result
              [((Result: t f o) (Result: t* f o))
               (subtype* A0 t t*)]
              ;; we can ignore interesting results
              [((Result: t f o) (Result: t* (FilterSet: (Top:) (Top:)) (Empty:)))
               (subtype* A0 t t*)]
              ;; subtyping on other stuff
              [((Syntax: t) (Syntax: t*))
               (subtype* A0 t t*)]
              [((Future: t) (Future: t*))
               (subtype* A0 t t*)]
              [((Instance: t) (Instance: t*))
               (subtype* A0 t t*)]
              [((Class: '() '() (list (and s  (list names  meths )) ...))
                (Class: '() '() (list (and s* (list names* meths*)) ...)))
               (for/fold ([A A0])
                         ([n names*] [m meths*])
                         (cond [(assq n s) => (lambda (spec) (subtype* A (cadr spec) m))]
                               [else (fail! s t)]))]
              ;; otherwise, not a subtype
              [(_ _) (fail! s t) #;(dprintf "failed")])))]))))

  (define (type-compare? a b)
  (and (subtype a b) (subtype b a)))


(provide/cond-contract
 [subtype (c:-> (c:or/c Type/c Values?) (c:or/c Type/c Values?) boolean?)])
(provide
  type-compare? subtypes/varargs subtypes)

;(trace subtype*)
;(trace supertype-of-one/arr)
;(trace arr-subtype*/no-fail)
;(trace subtype*/no-fail)
;(trace subtypes*)
;(trace subtype)

;(subtype (-> Univ B) (-> Univ Univ))
;(subtype (make-poly '(a) (make-tvar 'a)) (make-lst N))

;;problem:
;; (subtype (make-Mu 'x (make-Syntax (make-Union (list (make-Base 'Number #'number? number? #'-Number) (make-F 'x))))) (make-Syntax (make-Univ)))
