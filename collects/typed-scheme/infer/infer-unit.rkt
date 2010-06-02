#lang scheme/unit

(require scheme/require
         (except-in 
          (path-up
           "utils/utils.rkt" "utils/tc-utils.rkt" "types/utils.rkt"
           "rep/free-variance.rkt" "rep/type-rep.rkt" "rep/filter-rep.rkt" "rep/rep-utils.rkt"
           "types/convenience.rkt" "types/union.rkt" "types/subtype.rkt" "types/remove-intersect.rkt" "types/resolve.rkt"
           "env/type-name-env.rkt" "env/index-env.rkt" "env/tvar-env.rkt")
          make-env -> ->* one-of/c)
         "constraint-structs.rkt"
	 "signatures.rkt"                  
         scheme/match
         mzlib/etc
         mzlib/trace racket/contract
	 unstable/sequence unstable/list unstable/debug
         scheme/list)

(import dmap^ constraints^ promote-demote^)
(export infer^)

(define (empty-set) '())  

(define current-seen (make-parameter (empty-set) #;pair?))

(define (seen-before s t) (cons (Type-seq s) (Type-seq t)))
(define (remember s t A) (cons (seen-before s t) A))
(define (seen? s t) (member (seen-before s t) (current-seen)))


(define (dmap-constraint dmap dbound v)
  (let ([dc (hash-ref dmap dbound #f)])
    (match dc
      [(struct dcon (fixed #f))
       (if (eq? dbound v)
           (no-constraint v)
           (hash-ref fixed v (no-constraint v)))]
      [(struct dcon (fixed rest))
       (if (eq? dbound v)
           rest
           (hash-ref fixed v (no-constraint v)))]
      [(struct dcon-dotted (type bound))
       (if (eq? bound v)
           type
           (no-constraint v))]
      [_ (no-constraint v)])))

(define (map/cset f cset)
  (make-cset (for/list ([(cmap dmap) (in-pairs (cset-maps cset))])
               (f cmap dmap))))

(define (singleton-dmap dbound dcon)
  (make-dmap (make-immutable-hash (list (cons dbound dcon)))))

(define (hash-remove* hash keys)
  (for/fold ([h hash]) ([k (in-list keys)]) (hash-remove h k)))

(define (mover cset dbound vars f)
  (map/cset
   (lambda (cmap dmap)
     (cons (hash-remove* cmap vars)           
           (dmap-meet 
            (singleton-dmap 
             dbound
             (f cmap))
            dmap)))
   cset))

(define (move-vars-to-dmap cset dbound vars)
  (mover cset dbound vars
         (λ (cmap)
           (make-dcon (for/list ([v vars])
                        (hash-ref cmap v 
                                  (λ () (int-err "No constraint for new var ~a" v))))
                      #f))))

(define (move-rest-to-dmap cset dbound #:exact [exact? #f])
  (mover cset dbound (list dbound)
         (λ (cmap)
           ((if exact? make-dcon-exact make-dcon)
            null
            (hash-ref cmap dbound
                      (λ () (int-err "No constraint for bound ~a" dbound)))))))

(define (move-vars+rest-to-dmap cset dbound vars #:exact [exact? #f])
  (mover cset dbound vars
         (λ (cmap)
           ((if exact? make-dcon-exact make-dcon)
            (for/list ([v vars])
              (hash-ref cmap v (λ () (int-err "No constraint for new var ~a" v))))
            (hash-ref cmap dbound
                      (λ () (int-err "No constraint for bound ~a" dbound)))))))

;; s and t must be *latent* filters
(define (cgen/filter V X s t)
  (match* (s t)
    [(e e) (empty-cset X)]
    [(e (Top:)) (empty-cset X)]
    ;; FIXME - is there something to be said about the logical ones?
    [((TypeFilter: s p i) (TypeFilter: t p i)) (cset-meet (cgen V X s t) (cgen V X t s))]
    [((NotTypeFilter: s p i) (NotTypeFilter: t p i)) (cset-meet (cgen V X s t) (cgen V X t s))]
    [(_ _) (fail! s t)]))

;; s and t must be *latent* filter sets
(define (cgen/filter-set V X s t)
  (match* (s t)
    [(e e) (empty-cset X)]
    [((FilterSet: s+ s-) (FilterSet: t+ t-))
     (cset-meet (cgen/filter V X s+ t+) (cgen/filter V X s- t-))]
    [(_ _) (fail! s t)]))

(define (cgen/object V X s t)
  (match* (s t)
    [(e e) (empty-cset X)]
    [(e (Empty:)) (empty-cset X)]
    ;; FIXME - do something here    
    [(_ _) (fail! s t)]))

(define (cgen/arr V X s-arr t-arr)
  (define (cg S T) (cgen V X S T))
  (match* (s-arr t-arr)
    ;; the simplest case - no rests, drests, keywords
    [((arr: ss s #f #f '())
      (arr: ts t #f #f '()))
     (cset-meet* (list 
                  ;; contravariant
                  (cgen/list V X ts ss)
                  ;; covariant
                  (cg s t)))]
    ;; just a rest arg, no drest, no keywords
    [((arr: ss s s-rest #f '())
      (arr: ts t t-rest #f '()))
     (let ([arg-mapping 
            (cond 
              ;; both rest args are present, so make them the same length
              [(and s-rest t-rest)
               (cgen/list V X (cons t-rest (extend ss ts t-rest)) (cons s-rest (extend ts ss s-rest)))]
              ;; no rest arg on the right, so just pad the left and forget the rest arg
              [(and s-rest (not t-rest) (<= (length ss) (length ts)))
               (cgen/list V X ts (extend ts ss s-rest))]
              ;; no rest arg on the left, or wrong number = fail
              [else (fail! S T)])]
           [ret-mapping (cg s t)])
       (cset-meet* (list arg-mapping ret-mapping)))]
    ;; dotted on the left, nothing on the right
    [((arr: ss s #f (cons dty dbound) '())
      (arr: ts t #f #f                '()))
     (unless (memq dbound X)
       (fail! S T))
     (unless (<= (length ss) (length ts))
       (fail! S T))
     (let* ([vars      (for/list ([n (in-range (- (length ts) (length ss)))])
                         (gensym dbound))]
            [new-tys   (for/list ([var vars])
                         (substitute (make-F var) dbound dty))]
            [new-s-arr (make-arr (append ss new-tys) s #f #f null)]
            [new-cset  (cgen/arr V (append vars X) new-s-arr t-arr)])
       (move-vars-to-dmap new-cset dbound vars))]
    ;; dotted on the right, nothing on the left
    [((arr: ss s #f #f                '())
      (arr: ts t #f (cons dty dbound) '()))
     (unless (memq dbound X)
       (fail! S T))
     (unless (<= (length ts) (length ss))
       (fail! S T))
     (let* ([vars     (for/list ([n (in-range (- (length ss) (length ts)))])
                        (gensym dbound))]
            [new-tys  (for/list ([var vars])
                        (substitute (make-F var) dbound dty))]
            [new-t-arr (make-arr (append ts new-tys) t #f #f null)]
            [new-cset (cgen/arr V (append vars X) s-arr new-t-arr)])
       (move-vars-to-dmap new-cset dbound vars))]
    ;; this case is just for constrainting other variables, not dbound
    [((arr: ss s #f (cons s-dty dbound) '())
      (arr: ts t #f (cons t-dty dbound) '()))
     (unless (= (length ss) (length ts))
       (fail! S T))
     ;; If we want to infer the dotted bound, then why is it in both types?
     (when (memq dbound X)
       (fail! S T))
     (let* ([arg-mapping (cgen/list V X ts ss)]
            [darg-mapping (cgen V X t-dty s-dty)]
            [ret-mapping (cg s t)])
       (cset-meet* 
        (list arg-mapping darg-mapping ret-mapping)))]
    ;; bounds are different
    [((arr: ss s #f (cons s-dty dbound)  '())
      (arr: ts t #f (cons t-dty dbound*) '()))
     (unless (= (length ss) (length ts))
       (fail! S T))
     (let* ([arg-mapping (cgen/list V X ts ss)]
            ;; just add dbound as something that can be constrained
            [darg-mapping (cgen V (cons dbound X) t-dty s-dty)]
            [ret-mapping (cg s t)])
       (cset-meet* 
        (list arg-mapping darg-mapping ret-mapping)))]
    [((arr: ss s s-rest #f                  '())
      (arr: ts t #f     (cons t-dty dbound) '()))
     (unless (memq dbound X)
       (fail! S T))
     (if (<= (length ss) (length ts))
         ;; the simple case
         (let* ([arg-mapping (cgen/list V X ts (extend ts ss s-rest))]
                [darg-mapping (move-rest-to-dmap (cgen V X t-dty s-rest) dbound)]
                [ret-mapping (cg s t)])
           (cset-meet* (list arg-mapping darg-mapping ret-mapping)))
         ;; the hard case
         (let* ([vars     (for/list ([n (in-range (- (length ss) (length ts)))])
                            (gensym dbound))]
                [new-tys  (for/list ([var vars])
                            (substitute (make-F var) dbound t-dty))]
                [new-t-arr (make-arr (append ts new-tys) t #f (cons t-dty dbound) null)]
                [new-cset (cgen/arr V (append vars X) s-arr new-t-arr)])
           (move-vars+rest-to-dmap new-cset dbound vars)))]
    ;; If dotted <: starred is correct, add it below.  Not sure it is.
    [((arr: ss s #f     (cons s-dty dbound) '())
      (arr: ts t t-rest #f                  '()))
     (unless (memq dbound X)
       (fail! S T))
     (cond [(< (length ss) (length ts))
            ;; the hard case
            (let* ([vars     (for/list ([n (in-range (- (length ts) (length ss)))])
                               (gensym dbound))]
                   [new-tys  (for/list ([var vars])
                               (substitute (make-F var) dbound s-dty))]
                   [new-s-arr (make-arr (append ss new-tys) s #f (cons s-dty dbound) null)]
                   [new-cset (cgen/arr V (append vars X) new-s-arr t-arr)])
              (move-vars+rest-to-dmap new-cset dbound vars #:exact #t))]
           [else
            ;; the simple case
            (let* ([arg-mapping (cgen/list V X (extend ss ts t-rest) ss)]
                   [darg-mapping (move-rest-to-dmap (cgen V X t-rest s-dty) dbound #:exact #t)]
                   [ret-mapping (cg s t)])
              (cset-meet* (list arg-mapping darg-mapping ret-mapping)))])]
    [(_ _) (fail! S T)]))

;; V : a set of variables not to mention in the constraints
;; X : the set of variables to be constrained
;; S : a type to be the subtype of T
;; T : a type
;; produces a cset which determines a substitution that makes S a subtype of T
;; implements the V |-_X S <: T => C judgment from Pierce+Turner
(d/c (cgen V X S T)
  ((listof symbol?) (listof symbol?) Type? Type? . -> . cset?)
  ;; useful quick loop
  (define (cg S T) (cgen V X S T))
  ;; this places no constraints on any variables in X
  (define empty (empty-cset X))
  ;; this constrains just x (which is a single var)
  (define (singleton S x T)
    (insert empty x S T))
  ;; if we've been around this loop before, we're done (for rec types)
  (if (seen? S T)
      empty
      (parameterize ([match-equality-test (lambda (a b) (if (and (Rep? a) (Rep? b)) (type-equal? a b) (equal? a b)))]
                     ;; remember S and T, and obtain everything we've seen from the context
                     ;; we can't make this an argument since we may call back and forth with subtyping, for example
                     [current-seen (remember S T (current-seen))])
        (match* (S T)
          ;; if they're equal, no constraints are necessary (CG-Refl)
          [(a a) empty]
          ;; CG-Top
          [(_ (Univ:)) empty]

          ;; refinements are erased to their bound
          [((Refinement: S _ _) T)
           (cg S T)]

          ;; variables that are in X and should be constrained
          ;; all other variables are compatible only with themselves
          [((F: (? (λ (e) (memq e X)) v)) T)
           (match T
             ;; only possible when v* is an index
             [(F: v*) (when (and (bound-index? v*) (not (bound-tvar? v*)))
                        (fail! S T))]
             [_ (void)])
           ;; constrain v to be below T (but don't mention V)
           (singleton (Un) v (var-demote T V))]
          [(S (F: (? (lambda (e) (memq e X)) v)))
           (match S
             [(F: v*) (when (and (bound-index? v*) (not (bound-tvar? v*)))
                        (fail! S T))]
             [_ (void)])
           ;; constrain v to be above S (but don't mention V)
           (singleton (var-promote S V) v Univ)]
          
          ;; constrain b1 to be below T, but don't mention the new vars
          [((Poly: v1 b1) T) (cgen (append v1 V) X b1 T)]
          
          ;; constrain *each* element of es to be below T, and then combine the constraints
          [((Union: es) T) (cset-meet* (cons empty (for/list ([e es]) (cg e T))))]
          
          ;; find *an* element of es which can be made to be a supertype of S
          ;; FIXME: we're using multiple csets here, but I don't think it makes a difference 
          ;; not using multiple csets will break for: ???
          [(S (Union: es)) 
           (cset-combine
            (filter values
                    (for/list ([e es]) 
                      (with-handlers ([exn:infer? (λ _ #f)]) (cg S e)))))]
          
          ;; two structs with the same name and parent
          ;; just check pairwise on the fields
          ;; FIXME - wrong for mutable structs!
          [((Struct: nm p flds proc _ _ _ _ _) (Struct: nm p flds* proc* _ _ _ _ _))
           (let-values ([(flds flds*)
                         (cond [(and proc proc*)
                                (values (cons proc flds) (cons proc* flds*))]
                               [else (values flds flds*)])])
             (cgen/list V X flds flds*))]
          
          ;; two struct names, need to resolve b/c one could be a parent
          [((Name: n) (Name: n*))
           (if (free-identifier=? n n*)
               null
               (cg (resolve-once S) (resolve-once T)))]
          ;; pairs are pointwise
          [((Pair: a b) (Pair: a* b*))
           (cset-meet (cg a a*) (cg b b*))]
          ;; sequences are covariant
          [((Sequence: ts) (Sequence: ts*))
           (cgen/list V X ts ts*)]
          [((Listof: t) (Sequence: (list t*)))
           (cg t t*)]
          [((List: ts) (Sequence: (list t*)))
           (cset-meet* (for/list ([t (in-list ts)])
                         (cg t t*)))]
          [((HeterogenousVector: ts) (Sequence: (list t*)))
           (cset-meet* (for/list ([t (in-list ts)])
                         (cg t t*)))]
          [((Vector: t) (Sequence: (list t*)))
           (cg t t*)]
          [((Base: 'String _) (Sequence: (list t*)))
           (cg -Char t*)]
          [((Base: 'Bytes _) (Sequence: (list t*)))
           (cg -Nat t*)]
          [((Base: 'Input-Port _) (Sequence: (list t*)))
           (cg -Nat t*)]
          [((Vector: t) (Sequence: (list t*)))
           (cg t t*)]
          [((Hashtable: k v) (Sequence: (list k* v*)))
           (cgen/list V X (list k v) (list k* v*))]
          ;; ListDots can be below a Listof
          ;; must be above mu unfolding
          [((ListDots: s-dty dbound) (Listof: t-elem))
           (when (memq dbound X) (fail! S T))
           (cgen V X (substitute Univ dbound s-dty) t-elem)]
          ;; two ListDots with the same bound, just check the element type
          [((ListDots: s-dty dbound) (ListDots: t-dty dbound))
           (when (memq dbound X) (fail! S T))
           (cgen V X s-dty t-dty)]
          
          ;; this constrains `dbound' to be |ts| - |ss|
          [((ListDots: s-dty dbound) (List: ts))
           (unless (memq dbound X) (fail! S T))
           
           (let* ([vars     (for/list ([n (in-range (length ts))])
                              (gensym dbound))]
                  ;; new-tys are dummy plain type variables, standing in for the elements of dbound that need to be generated
                  [new-tys  (for/list ([var vars])
                              (substitute (make-F var) dbound s-dty))]
                  ;; generate constraints on the prefixes, and on the dummy types
                  [new-cset (cgen/list V (append vars X) new-tys ts)])
             ;; now take all the dummy types, and use them to constrain dbound appropriately
             (move-vars-to-dmap new-cset dbound vars))]
          
          ;; if we have two mu's, we rename them to have the same variable
          ;; and then compare the bodies
          ;; This relies on (B 0) only unifying with itself, and thus only hitting the first case of this `match'
          [((Mu-unsafe: s) (Mu-unsafe: t)) 
           (cg s t)]
          
          ;; other mu's just get unfolded
          [(s (? Mu? t)) (cg s (unfold t))]
          [((? Mu? s) t) (cg (unfold s) t)]
                    
          ;; resolve applications
          [((App: _ _ _) _) (cg (resolve-once S) T)]
          [(_ (App: _ _ _)) (cg S (resolve-once T))]
          
          ;; values are covariant
          [((Values: ss) (Values: ts))
           (unless (= (length ss) (length ts))
             (fail! ss ts))
           (cgen/list V X ss ts)]
          
          ;; this constrains `dbound' to be |ts| - |ss|
          [((ValuesDots: ss s-dty dbound) (Values: ts))
           (unless (>= (length ts) (length ss)) (fail! ss ts))
           (unless (memq dbound X) (fail! S T))
           
           (let* ([vars     (for/list ([n (in-range (- (length ts) (length ss)))])
                              (gensym dbound))]
                  ;; new-tys are dummy plain type variables, standing in for the elements of dbound that need to be generated
                  [new-tys  (for/list ([var vars])
                              (substitute (make-F var) dbound s-dty))]
                  ;; generate constraints on the prefixes, and on the dummy types
                  [new-cset (cgen/list V (append vars X) (append ss new-tys) ts)])
             ;; now take all the dummy types, and use them to constrain dbound appropriately
             (move-vars-to-dmap new-cset dbound vars))]
          
          ;; identical bounds - just unify pairwise
          [((ValuesDots: ss s-dty dbound) (ValuesDots: ts t-dty dbound))
           (when (memq dbound X) (fail! ss ts))          
           (cgen/list V X (cons s-dty ss) (cons t-dty ts))]
          ;; vectors are invariant - generate constraints *both* ways
          [((Vector: e) (Vector: e*))
           (cset-meet (cg e e*) (cg e* e))]
          ;; boxes are invariant - generate constraints *both* ways
          [((Box: e) (Box: e*))
           (cset-meet (cg e e*) (cg e* e))]
          [((MPair: s t) (MPair: s* t*))
           (cset-meet* (list (cg s s*) (cg s* s) (cg t t*) (cg t* t)))]
          [((Channel: e) (Channel: e*))
           (cset-meet (cg e e*) (cg e* e))]
          ;; we assume all HTs are mutable at the moment
          [((Hashtable: s1 s2) (Hashtable: t1 t2))
           ;; for mutable hash tables, both are invariant
           (cset-meet* (list (cg t1 s1) (cg s1 t1) (cg t2 s2) (cg s2 t2)))]
          ;; syntax is covariant
          [((Syntax: s1) (Syntax: s2))
           (cg s1 s2)]
          ;; parameters are just like one-arg functions
          [((Param: in1 out1) (Param: in2 out2))
           (cset-meet (cg in2 in1) (cg out1 out2))]
          ;; every function is trivially below top-arr
          [((Function: _)
            (Function: (list (top-arr:))))
           empty]
          [((Function: (list s-arr ...))
            (Function: (list t-arr ...)))
           (cset-meet*
            (for/list ([t-arr t-arr])
              ;; for each element of t-arr, we need to get at least one element of s-arr that works
              (let ([results (filter values
                                     (for/list ([s-arr s-arr])
                                       (with-handlers ([exn:infer? (lambda (_) #f)])
                                         (cgen/arr V X s-arr t-arr))))])
                ;; ensure that something produces a constraint set
                (when (null? results) (fail! S T))
                (cset-combine results))))]
          ;; check each element
          [((Result: s f-s o-s)
            (Result: t f-t o-t))
           (cset-meet* (list (cg s t) 
                             (cgen/filter-set V X f-s f-t)
                             (cgen/object V X o-s o-t)))]
          [(_ _)
           (cond 
             ;; subtypes are easy - should this go earlier?
             [(subtype S T) empty]
             ;; or, nothing worked, and we fail
             [else (fail! S T)])]))))

(d/c (subst-gen C R)
  (cset? Type? . -> . (or/c #f list?))
  ;; fixme - should handle these separately
  (define must-vars (append (fv R) (fi R)))
  (define (constraint->type v #:variable [variable #f])
    (match v
      [(struct c (S X T))
       ;; fixme - handle free indexes, remove Dotted
       (let ([var (hash-ref (free-vars* R) (or variable X) Constant)])
         ;(printf "variance was: ~a~nR was ~a~nX was ~a~nS T ~a ~a~n" var R (or variable X) S T)
         (evcase var 
                 [Constant S]
                 [Covariant S]
                 [Contravariant T]
                 [Invariant S]
                 [Dotted T]))]))  
  (match (car (cset-maps C))
    [(cons cmap (dmap dm))
     (let ([subst (append 
                   (for/list ([(k dc) (in-hash dm)])
                     (match dc
                       [(dcon fixed rest)
                        (list k
                              (for/list ([f fixed])
                                (constraint->type f #:variable k))
                              (and rest (constraint->type rest)))]
                       [(dcon-exact fixed rest)
                        (list k
                              (for/list ([f fixed])
                                (constraint->type f #:variable k))
                              (constraint->type rest))]))
                   (for/list ([(k v) (in-hash cmap)])
                     (list k (constraint->type v))))])
       ;; verify that we got all the important variables
       (and (for/and ([v must-vars])
              (assq v subst))
            subst))]))

;; V : a set of variables not to mention in the constraints
;; X : the set of variables to be constrained
;; S : a list of types to be the subtypes of T
;; T : a list of types
;; produces a cset which determines a substitution that makes the Ss subtypes of the Ts
(d/c (cgen/list V X S T)
  ((listof symbol?) (listof symbol?) (listof Type?) (listof Type?) . -> . cset?)
  (unless (= (length S) (length T))
    (fail! S T))
  (cset-meet* (for/list ([s S] [t T]) (cgen V X s t))))

;; X : variables to infer
;; Y : indices to infer
;; S : actual argument types
;; T : formal argument types
;; R : result type
;; expected : boolean
;; returns a substitution
;; if R is #f, we don't care about the substituion
;; just return a boolean result
(define (infer X Y S T R [expected #f])
  (with-handlers ([exn:infer? (lambda _ #f)])  
    (let* ([cs (cgen/list null (append X Y) S T)]
           [cs* (if expected 
                    (cset-meet cs (cgen null (append X Y) R expected))
                    cs)])
          (if R (subst-gen cs* R) #t))))

;; like infer, but T-var is the vararg type:
(define (infer/vararg X Y S T T-var R [expected #f])
  (define new-T (if T-var (extend S T T-var) T))
  (and ((length S) . >= . (length T))
       (infer X Y S new-T R expected)))

;; like infer, but dotted-var is the bound on the ...
;; and T-dotted is the repeated type
(define (infer/dots X dotted-var S T T-dotted R must-vars #:expected [expected #f])
  (with-handlers ([exn:infer? (lambda _ #f)])
    (let* ([short-S (take S (length T))]
           [rest-S (drop S (length T))]
           [cs-short (cgen/list null (cons dotted-var X) short-S T)]
           [new-vars (for/list ([i (in-range (length rest-S))]) (gensym dotted-var))]
           [new-Ts (for/list ([v new-vars])
                     (substitute (make-F v) dotted-var 
                                 (substitute-dots (map make-F new-vars) #f dotted-var T-dotted)))]
           [cs-dotted (cgen/list null (append new-vars X) rest-S new-Ts)]
           [cs-dotted* (move-vars-to-dmap cs-dotted dotted-var new-vars)]
           [cs (cset-meet cs-short cs-dotted*)])
      (if (not expected)
          (subst-gen cs R)
          (subst-gen (cset-meet cs (cgen null X R expected)) R)))))

;(trace cgen)
