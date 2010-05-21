#lang scheme/unit

(require scheme/require
         (except-in 
          (path-up
           "utils/utils.rkt" "utils/tc-utils.rkt"
           "rep/free-variance.rkt" "rep/type-rep.rkt" "rep/filter-rep.rkt" "rep/rep-utils.rkt"
           "types/convenience.rkt" "types/union.rkt" "types/subtype.rkt" "types/remove-intersect.rkt" "types/resolve.rkt"
           "env/type-name-env.rkt")
          make-env)
         (except-in (path-up "types/utils.rkt") Dotted)
         (only-in (path-up "env/type-env-structs.rkt" "env/tvar-env.rkt") lookup current-tvars)
         "constraint-structs.rkt"
	 "signatures.rkt"                  
         scheme/match
         mzlib/etc
         mzlib/trace
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
         (lambda (cmap)
           (make-dcon (for/list ([v vars])
                        (hash-ref cmap v 
                                  (lambda () (int-err "No constraint for new var ~a" v))))
                      #f))))

(define (move-rest-to-dmap cset dbound #:exact [exact? #f])
  (mover cset dbound (list dbound)
         (lambda (cmap)
           ((if exact? make-dcon-exact make-dcon)
            null
            (hash-ref cmap dbound
                      (lambda () (int-err "No constraint for bound ~a" dbound)))))))

(define (move-vars+rest-to-dmap cset dbound vars #:exact [exact? #f])
  (map/cset
   (lambda (cmap dmap)
     (cons (hash-remove* cmap vars)           
            (dmap-meet 
             (singleton-dmap 
              dbound
              ((if exact? make-dcon-exact make-dcon)
               (for/list ([v vars])
                 (hash-ref cmap v 
                           (lambda () (int-err "No constraint for new var ~a" v))))
               (hash-ref cmap dbound
                         (lambda () (int-err "No constraint for bound ~a" dbound)))))
             dmap)))
   cset))

;; t and s must be *latent* filters
(define (cgen/filter V X t s)
  (match* (t s)
    [(e e) (empty-cset X)]
    [(e (Top:)) (empty-cset X)]
    ;; FIXME - is there something to be said about the logical ones?
    [((TypeFilter: t p i) (TypeFilter: s p i)) (cset-meet (cgen V X t s) (cgen V X s t))]
    [((NotTypeFilter: t p i) (NotTypeFilter: s p i)) (cset-meet (cgen V X t s) (cgen V X s t))]
    [(_ _) (fail! t s)]))

#;
(define (cgen/filters V X ts ss)
  (cond 
    [(null? ss) (empty-cset X)]
    ;; FIXME - this can be less conservative
    [(= (length ts) (length ss))
     (cset-meet* (for/list ([t ts] [s ss]) (cgen/filter V X t s)))]
    [else (fail! ts ss)]))


;; t and s must be *latent* filter sets
(define (cgen/filter-set V X t s)
  (match* (t s)
    [(e e) (empty-cset X)]
    [((FilterSet: t+ t-) (FilterSet: s+ s-))
     (cset-meet (cgen/filter V X t+ s+) (cgen/filter V X t- s-))]
    [(_ _) (fail! t s)]))

(define (cgen/object V X t s)
  (match* (t s)
    [(e e) (empty-cset X)]
    [(e (Empty:)) (empty-cset X)]
    ;; FIXME - do something here    
    [(_ _) (fail! t s)]))

(define (cgen/arr V X t-arr s-arr)
  (define (cg S T) (cgen V X S T))
  (match* (t-arr s-arr)
    [((arr: ts t #f #f '())
      (arr: ss s #f #f '()))
     (cset-meet* 
      (list (cgen/list V X ss ts)
            (cg t s)))]
    [((arr: ts t t-rest #f '())
      (arr: ss s s-rest #f '()))
     (let ([arg-mapping 
            (cond [(and t-rest s-rest (<= (length ts) (length ss)))
                   (cgen/list V X (cons s-rest ss) (cons t-rest (extend ss ts t-rest)))]
                  [(and t-rest s-rest (>= (length ts) (length ss)))
                   (cgen/list V X (cons s-rest (extend ts ss s-rest)) (cons t-rest ts))]
                  [(and t-rest (not s-rest) (<= (length ts) (length ss)))
                   (cgen/list V X ss (extend ss ts t-rest))]
                  [(and s-rest (not t-rest) (>= (length ts) (length ss)))
                   (cgen/list V X (extend ts ss s-rest) ts)]
                  [else (fail! S T)])]
           [ret-mapping (cg t s)])
       (cset-meet*
        (list arg-mapping ret-mapping)))]
    [((arr: ts t #f (cons dty dbound) '())
      (arr: ss s #f #f                '()))
     (unless (memq dbound X)
       (fail! S T))
     (unless (<= (length ts) (length ss))
       (fail! S T))
     (let* ([num-vars (- (length ss) (length ts))]
            [vars     (for/list ([n (in-range num-vars)])
                        (gensym dbound))]
            [new-tys  (for/list ([var vars])
                        (substitute (make-F var) dbound dty))]
            [new-cset (cgen/arr V (append vars X) (make-arr (append ts new-tys) t #f #f null) s-arr)])
       (move-vars-to-dmap new-cset dbound vars))]
    [((arr: ts t #f #f                '())
      (arr: ss s #f (cons dty dbound) '()))
     (unless (memq dbound X)
       (fail! S T))
     (unless (<= (length ss) (length ts))
       (fail! S T))
     (let* ([num-vars (- (length ts) (length ss))]
            [vars     (for/list ([n (in-range num-vars)])
                        (gensym dbound))]
            [new-tys  (for/list ([var vars])
                        (substitute (make-F var) dbound dty))]
            [new-cset (cgen/arr V (append vars X) t-arr (make-arr (append ss new-tys) s #f #f null))])
       (move-vars-to-dmap new-cset dbound vars))]
    [((arr: ts t #f (cons t-dty dbound) '())
      (arr: ss s #f (cons s-dty dbound) '()))
     (unless (= (length ts) (length ss))
       (fail! S T))
     ;; If we want to infer the dotted bound, then why is it in both types?
     (when (memq dbound X)
       (fail! S T))
     (let* ([arg-mapping (cgen/list V X ss ts)]
            [darg-mapping (cgen V X s-dty t-dty)]
            [ret-mapping (cg t s)])
       (cset-meet* 
        (list arg-mapping darg-mapping ret-mapping)))]
    [((arr: ts t #f (cons t-dty dbound)  '())
      (arr: ss s #f (cons s-dty dbound*) '()))
     (unless (= (length ts) (length ss))
       (fail! S T))
     (let* ([arg-mapping (cgen/list V X ss ts)]
            [darg-mapping (cgen V (cons dbound* X) s-dty t-dty)]
            [ret-mapping (cg t s)])
       (cset-meet* 
        (list arg-mapping darg-mapping ret-mapping)))]
    [((arr: ts t t-rest #f                  '())
      (arr: ss s #f     (cons s-dty dbound) '()))
     (unless (memq dbound X)
       (fail! S T))
     (if (<= (length ts) (length ss))
         ;; the simple case
         (let* ([arg-mapping (cgen/list V X ss (extend ss ts t-rest))]
                [darg-mapping (move-rest-to-dmap (cgen V X s-dty t-rest) dbound)]
                [ret-mapping (cg t s)])
           (cset-meet* (list arg-mapping darg-mapping ret-mapping)))
         ;; the hard case
         (let* ([num-vars (- (length ts) (length ss))]
                [vars     (for/list ([n (in-range num-vars)])
                            (gensym dbound))]
                [new-tys  (for/list ([var vars])
                            (substitute (make-F var) dbound s-dty))]
                [new-cset (cgen/arr V (append vars X) t-arr 
                                    (make-arr (append ss new-tys) s #f (cons s-dty dbound) null))])
           (move-vars+rest-to-dmap new-cset dbound vars)))]
    ;; If dotted <: starred is correct, add it below.  Not sure it is.
    [((arr: ts t #f     (cons t-dty dbound) '())
      (arr: ss s s-rest #f                  '()))
     (unless (memq dbound X)
       (fail! S T))
     (cond [(< (length ts) (length ss))
            ;; the hard case
            (let* ([num-vars (- (length ss) (length ts))]
                   [vars     (for/list ([n (in-range num-vars)])
                               (gensym dbound))]
                   [new-tys  (for/list ([var vars])
                               (substitute (make-F var) dbound t-dty))]
                   [arg-mapping (cgen/list V (append vars X) ss (append ts new-tys))]
                   [darg-mapping (cgen V X s-rest t-dty)]
                   [ret-mapping (cg t s)]
                   [new-cset
                    (cset-meet* (list arg-mapping darg-mapping ret-mapping))])
              (move-vars+rest-to-dmap new-cset dbound vars #:exact #t))]
           [else
            ;; the simple case
            (let* ([arg-mapping (cgen/list V X (extend ts ss s-rest) ts)]
                   [darg-mapping (move-rest-to-dmap (cgen V X s-rest t-dty) dbound #:exact #t)]
                   [ret-mapping (cg t s)])
              (cset-meet* (list arg-mapping darg-mapping ret-mapping)))])]
    [(_ _) (fail! S T)]))

;; determine constraints on the variables in X that would make T a supertype of S
;; the resulting constraints will not mention V
(define (cgen V X S T)
  (define (cg S T) (cgen V X S T))
  (define empty (empty-cset X))
  (define (singleton S X T)
    (insert empty X S T))
  (if (seen? S T)
      empty
      (parameterize ([match-equality-test (lambda (a b) (if (and (Rep? a) (Rep? b)) (type-equal? a b) (equal? a b)))]
                     [current-seen (remember S T (current-seen))])
        (match* 
            (S T)
          [(a a) empty]
          [(_ (Univ:)) empty]

          [((Refinement: S _ _) T)
           (cg S T)]

          [((F: (? (lambda (e) (memq e X)) v)) S)
           (when (match S
                   [(F: v*)
                    (just-Dotted? (lookup (current-tvars) v* (lambda _ #f)))]
                   [_ #f])
             (fail! S T))
           (singleton (Un) v (var-demote S V))]
          [(S (F: (? (lambda (e) (memq e X)) v)))
           (when (match S
                   [(F: v*)
                    (just-Dotted? (lookup (current-tvars) v* (lambda _ #f)))]
                   [_ #f])
             (fail! S T))
           (singleton (var-promote S V) v Univ)]

          ;; two unions with the same number of elements, so we just try to unify them pairwise
          #;[((Union: l1) (Union: l2))
             (=> unmatch)
             (unless (= (length l1) (length l2))
               (unmatch))
             (cgen-union V X l1 l2)]
          
          #;[((Poly: v1 b1) (Poly: v2 b2))
             (unless (= (length v1) (length v2))
               (fail! S T))
             (let ([b2* (subst-all (map list v2 v1) b2)])
               (cg b1 b2*))]
          
          #;[((PolyDots: (list v1 ... r1) b1) (PolyDots: (list v2 ... r2) b2))
             (unless (= (length v1) (length v2))
               (fail! S T))
             (let ([b2* (substitute-dotted v1 v1 v2 (subst-all (map list v2 v1) b2))])
               (cg b1 b2*))]
          
          [((Poly: v1 b1) T)
           (cgen (append v1 V) X b1 T)]
          
          #;[((PolyDots: (list v1 ... r1) b1) T)
             (let ([b1* (var-demote b1 (cons r1 v1))])
               (cg b1* T))]
          
          #;
          [((Poly-unsafe: n b) (Poly-unsafe: n* b*))
           (unless (= n n*)
             (fail! S T))
           (cg b b*)]
          
          
          [((Union: es) S) (cset-meet* (cons empty (for/list ([e es]) (cg e S))))]
          ;; we might want to use multiple csets here, but I don't think it makes a difference
          [(S (Union: es)) (or
                            (for/or 
                             ([e es]) 
                             (with-handlers
                                 ([exn:infer? (lambda _ #f)])
                               (cg S e)))
                            (fail! S T))]
          
          [((Struct: nm p flds proc _ _ _ _ _) (Struct: nm p flds* proc* _ _ _ _ _))
           (let-values ([(flds flds*)
                         (cond [(and proc proc*)
                                (values (cons proc flds) (cons proc* flds*))]
                               [(or proc proc*)
                                (fail! S T)]
                               [else (values flds flds*)])])
             (cgen/list V X flds flds*))]
          [((Name: n) (Name: n*))
           (if (free-identifier=? n n*)
               null
               (fail! S T))]
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
          ;; if we have two mu's, we rename them to have the same variable
          ;; and then compare the bodies
          [((Mu-unsafe: s) (Mu-unsafe: t)) 
           (cg s t)]
          ;; other mu's just get unfolded
          [(s (? Mu? t)) (cg s (unfold t))]
          [((? Mu? s) t) (cg (unfold s) t)]
          ;; type application          
          [((App: (Name: n) args _)
            (App: (Name: n*) args* _))
           (unless (free-identifier=? n n*)
             (fail! S T))
             (cg (resolve-once S) (resolve-once T))]
          [((App: _ _ _) _) (cg (resolve-once S) T)]
          [(_ (App: _ _ _)) (cg S (resolve-once T))]
          [((Values: ss) (Values: ts))
           (unless (= (length ss) (length ts))
             (fail! ss ts))
           (cgen/list V X ss ts)]
          [((Values: ss) (ValuesDots: ts t-dty dbound))
           (unless (>= (length ss) (length ts))
             (fail! ss ts))
           (unless (memq dbound X)
             (fail! S T))
           (let* ([num-vars (- (length ss) (length ts))]
                  [vars     (for/list ([n (in-range num-vars)])
                              (gensym dbound))]
                  [new-tys  (for/list ([var vars])
                              (substitute (make-F var) dbound t-dty))]
                  [new-cset (cgen/list V (append vars X) ss (append ts new-tys))])
             (move-vars-to-dmap new-cset dbound vars))]
          [((ValuesDots: ss s-dty dbound) (Values: ts))
           (unless (>= (length ts) (length ss))
             (fail! ss ts))
           (unless (memq dbound X)
             (fail! S T))
           (let* ([num-vars (- (length ts) (length ss))]
                  [vars     (for/list ([n (in-range num-vars)])
                              (gensym dbound))]
                  [new-tys  (for/list ([var vars])
                              (substitute (make-F var) dbound s-dty))]
                  [new-cset (cgen/list V (append vars X) (append ss new-tys) ts)])
             (move-vars-to-dmap new-cset dbound vars))]
          [((ValuesDots: ss s-dty dbound) (ValuesDots: ts t-dty dbound))
           (when (memq dbound X) (fail! ss ts))          
           (cgen/list V X (cons s-dty ss) (cons t-dty ts))]          
          [((Vector: e) (Vector: e*))
           (cset-meet (cg e e*) (cg e* e))]
          [((Box: e) (Box: e*))
           (cset-meet (cg e e*) (cg e* e))]
          [((MPair: s t) (MPair: s* t*))
           (cset-meet* (list (cg s s*) (cg s* s) (cg t t*) (cg t* t)))]
          [((Channel: e) (Channel: e*))
           (cset-meet (cg e e*) (cg e* e))]
          [((Hashtable: s1 s2) (Hashtable: t1 t2))
           ;; for mutable hash tables, both are invariant
           (cset-meet* (list (cg t1 s1) (cg s1 t1) (cg t2 s2) (cg s2 t2)))]
          [((Syntax: s1) (Syntax: s2))
           (cg s1 s2)]
          ;; parameters are just like one-arg functions
          [((Param: in1 out1) (Param: in2 out2))
           (cset-meet (cg in2 in1) (cg out1 out2))]
          [((Function: _)
            (Function: (list (top-arr:))))
           empty]
          [((Function: (list t-arr ...))
            (Function: (list s-arr ...)))
           (=> unmatch)
           (cset-combine
            (filter 
             values ;; only generate the successful csets
             (for*/list 
              ([t-arr t-arr] [s-arr s-arr])
              (with-handlers ([exn:infer? (lambda (_) #f)])
                (cgen/arr V X t-arr s-arr)))))]
          ;; this is overly conservative
          [((Result: s f-s o-s)
            (Result: t f-t o-t))
           (cset-meet* (list (cg s t) 
                             (cgen/filter-set V X f-s f-t)
                             (cgen/object V X o-s o-t)))]
          [(_ _)
           (cond [(subtype S T) empty]
                 ;; or, nothing worked, and we fail
                 [else (fail! S T)])]))))

(define (check-vars must-vars subst)
  (and (for/and ([v must-vars])
                (assq v subst))
       subst))

(define (subst-gen C R must-vars)
  (define (constraint->type v #:variable [variable #f])
    (match v
      [(struct c (S X T))
       (let ([var (hash-ref (free-vars* R) (or variable X) Constant)])
         ;(printf "variance was: ~a~nR was ~a~nX was ~a~nS T ~a ~a~n" var R (or variable X) S T)
         (evcase var 
                 [Constant S]
                 [Covariant S]
                 [Contravariant T]
                 [Invariant S]
                 [Dotted T]))]))
  (match (car (cset-maps C))
    [(cons cmap (struct dmap (dm)))
     (check-vars
      must-vars
      (append 
       (for/list ([(k dc) (in-hash dm)])
         (match dc
           [(struct dcon (fixed rest))
            (list k
                  (for/list ([f fixed])
                    (constraint->type f #:variable k))
                  (and rest (constraint->type rest)))]
           [(struct dcon-exact (fixed rest))
            (list k
                  (for/list ([f fixed])
                    (constraint->type f #:variable k))
                  (constraint->type rest))]))
       (for/list ([(k v) (in-hash cmap)])
         (list k (constraint->type v)))))]))

(define (cgen/list V X S T)
  (unless (= (length S) (length T))
    (fail! S T))
  (cset-meet* (for/list ([s S] [t T]) (cgen V X s t))))

;; X : variables to infer
;; S : actual argument types
;; T : formal argument types
;; R : result type
;; must-vars : variables that must be in the substitution
;; expected : boolean
;; returns a substitution
;; if R is #f, we don't care about the substituion
;; just return a boolean result
(define (infer X S T R must-vars [expected #f])
  (with-handlers ([exn:infer? (lambda _ #f)])  
    (let ([cs (cgen/list null X S T)])
      (if (not expected)
          (subst-gen cs R must-vars)
          (subst-gen (cset-meet cs (cgen null X R expected)) R must-vars)))))

;; like infer, but T-var is the vararg type:
(define (infer/vararg X S T T-var R must-vars [expected #f])
  (define new-T (if T-var (extend S T T-var) T))
  (and ((length S) . >= . (length T))
       (infer X S new-T R must-vars expected)))

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
          (subst-gen cs R must-vars)
          (subst-gen (cset-meet cs (cgen null X R expected)) R must-vars)))))

(define (infer/simple S T R)
  (infer (fv/list T) S T R))

(define (i s t r)
  (infer/simple (list s) (list t) r))

;(trace cgen)
