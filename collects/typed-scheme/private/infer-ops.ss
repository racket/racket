#lang scheme/base

(require "type-effect-convenience.ss" "type-rep.ss" "effect-rep.ss" "rep-utils.ss"
         "free-variance.ss" "type-utils.ss" "union.ss" "tc-utils.ss" "type-name-env.ss"
         "subtype.ss" "remove-intersect.ss"
         scheme/match
         mzlib/etc
         mzlib/trace
         scheme/list)

(provide infer infer/vararg restrict)

(define-values (fail-sym exn:infer?)
  (let ([sym (gensym)])
    (values sym (lambda (s) (eq? s sym)))))

;; inference failure - masked before it gets to the user program
(define-syntax fail!
  (syntax-rules ()
    [(_ s t) (raise fail-sym)]))

(define (V-in? V . ts)
  (for/or ([e (append* (map fv ts))])
          (memq e V)))

(define (var-promote T V)
  (define (vp t) (var-promote t V))
  (define (inv t) (if (V-in? V t) Univ t))
  (type-case vp T
             [#:F name (if (memq name V) Univ T)]
             [#:Vector t (make-Vector (inv t))]
             [#:Box t (make-Box (inv t))]
             [#:Hashtable k v
                          (if (V-in? V v)
                              Univ
                              (make-Hashtable (vp k) v))]
             [#:Param in out
                          (make-Param (var-demote in V)
                                          (vp out))]
             [#:arr dom rng rest thn els
                    (if 
                     (apply V-in? V (append thn els))
                     (make-top-arr)
                     (make-arr (for/list ([d dom]) (var-demote d V))
                               (vp rng)
                               (and rest (var-demote rest V))
                               thn
                               els))]))

(define (var-demote T V)
  (define (vd t) (var-demote t V))
  (define (inv t) (if (V-in? V t) (Un) t))
  (type-case vd T
             [#:F name (if (memq name V) (Un) T)]
             [#:Vector t (make-Vector (inv t))]
             [#:Box t (make-Box (inv t))]
             [#:Hashtable k v
                          (if (V-in? V v)
                              (Un)
                              (make-Hashtable (vd k) v))]
             [#:Param in out
                          (make-Param (var-promote in V)
                                          (vd out))]
             [#:arr dom rng rest thn els
                    (if (apply V-in? V (append thn els))
                        (make-arr null (Un) Univ null null)
                        (make-arr (for/list ([d dom]) (var-promote d V))
                                  (vd rng)
                                  (and rest (var-promote rest V))
                                  thn
                                  els))]))
;; a stupid impl
(define (meet S T) 
  (let ([s* (restrict S T)])
    (if (and (subtype s* S)
             (subtype s* T))
        s*
        (Un))))

(define (join T U) (Un T U))

;; S, T types
;; X a var
(define-struct c (S X T) #:prefab)

;; map is a functional map from vars to c's
;; V list of vars
(define-struct cset (maps) #:prefab)

(define (empty-cset X)
  (make-cset (list (for/hash ([x X]) (values x (make-c (Un) x Univ))))))

#;
(define (lookup cset var)
  (hash-ref (cset-map cset) var (make-c (Un) var Univ)))

(define (insert cs var S T)
  (match cs
    [(struct cset (maps))
     (make-cset (for/list ([map maps])(hash-set map var (make-c S var T))))]))

(define c-meet
  (match-lambda**
   [((struct c (S X T)) (struct c (S* _ T*)))
    (let ([S (join S S*)] [T (meet T T*)])
      (unless (subtype S T)
        (fail! S T))
      (make-c S X T))]))
    

(define (cset-meet x y)
  (match* (x y)
   [((struct cset (maps1)) (struct cset (maps2)))
    (let ([maps (filter values
                        (for*/list
                         ([map1 maps1] [map2 maps2])
                         (with-handlers ([exn:infer? (lambda (_) #f)])
                           (for/hash ([(k v1) map1])                     
                                     (values k (c-meet v1 (hash-ref map2 k)))))))])
      (when (null? maps)
        (fail! maps1 maps2))
      (make-cset maps))]))

(define (cset-meet* V args)
  (for/fold ([c (empty-cset V)])
    ([a args])
    (cset-meet a c)))

;; ss and ts have the same length
(define (cgen-union V X ss ts)
  ;; first, we remove common elements of ss and ts
  (let-values ([(ss* ts*)
                (values (filter (lambda (se) (not (ormap (lambda (t) (type-equal? t se)) ts))) ss)
                        (filter (lambda (te) (not (ormap (lambda (s) (type-equal? s te)) ss))) ts))])
    (cgen/list V X ss* ts*)))

(define (cset-combine l)
  (let ([mapss (map cset-maps l)])
    (make-cset (apply append mapss))))

(define (empty-set) '())  

(define current-seen (make-parameter (empty-set)))

(define (seen-before s t) (cons (Type-seq s) (Type-seq t)))
(define (remember s t A) (cons (seen-before s t) A))
(define (seen? s t) (member (seen-before s t) (current-seen)))

(define (cgen V X S T)
  (define empty (empty-cset X))
  (define (singleton S X T )
    (insert empty X S T))
  (if (seen? S T)
      empty
      (parameterize ([match-equality-test type-equal?]
                     [current-seen (remember S T (current-seen))])
        (match* 
         (S T)
         [(a a) empty]
         [(_ (Univ:)) empty]
         
         [((F: (? (lambda (e) (memq e X)) v)) S)
          (singleton (Un) v (var-demote S V))]
         [(S (F: (? (lambda (e) (memq e X)) v)))
          (singleton (var-promote S V) v Univ)]
         
         
         ;; two unions with the same number of elements, so we just try to unify them pairwise
         #;[((Union: l1) (Union: l2))
            (=> unmatch)
            (unless (= (length l1) (length l2))
              (unmatch))
            (cgen-union V X l1 l2)]
         #;
         [((Poly-unsafe: n b) (Poly-unsafe: n* b*))
          (unless (= n n*)
            (fail! S T))
          (cgen V X b b*)]
         
         
         [((Union: es) S) (cset-meet* X (for/list ([e es]) (cgen V X e S)))]
         ;; we might want to use multiple csets here, but I don't think it makes a difference
         [(S (Union: es)) (or
                           (for/or 
                            ([e es]) 
                            (with-handlers
                                ([exn:infer? (lambda _ #f)])
                              (cgen V X S e)))
                           (fail! S T))]
         
         [((Struct: nm p flds proc _ _ _) (Struct: nm p flds* proc* _ _ _))
          (let-values ([(flds flds*)
                        (cond [(and proc proc*)
                               (values (cons proc flds) (cons proc* flds*))]
                              [(or proc proc*)
                               (fail! S T)]
                              [else (values flds flds*)])])
            (cgen/list X V flds flds*))]
         [((Name: n) (Name: n*))
          (if (free-identifier=? n n*)
              null
              (fail! S T))]
         [((Pair: a b) (Pair: a* b*))
          (cset-meet (cgen V X a a*) (cgen V X b b*))]
         ;; if we have two mu's, we rename them to have the same variable
         ;; and then compare the bodies
         [((Mu-unsafe: s) (Mu-unsafe: t)) 
          (cgen V X s t)]
         ;; other mu's just get unfolded
         [(s (? Mu? t)) (cgen V X s (unfold t))]
         [((? Mu? s) t) (cgen V X (unfold s) t)]
         ;; type application          
         [((App: (Name: n) args _)
           (App: (Name: n*) args* _))
          (unless (free-identifier=? n n*)
            (fail! S T))
          (let ([x (instantiate-poly (lookup-type-name n) args)]
                [y (instantiate-poly (lookup-type-name n) args*)])
            (cgen V X x y))]
         [((Vector: e) (Vector: e*))
          (cset-meet (cgen V X e e*) (cgen V X e* e))]
         [((Box: e) (Box: e*))
          (cset-meet (cgen V X e e*) (cgen V X e* e))]
         [((Hashtable: s1 s2) (Hashtable: t1 t2))
          ;; the key is covariant, the value is invariant
          (cset-meet* X (list (cgen V X s1 t1) (cgen V X t2 s2) (cgen V X s2 t2)))]
         [((Syntax: s1) (Syntax: s2))
          (cgen V X s1 s2)]
         ;; parameters are just like one-arg functions
         [((Param: in1 out1) (Param: in2 out2))
          (cset-meet (cgen V X in2 in1) (cgen V X out1 out2))]
         [((Function: (list t-arr ...))
           (Function: (list s-arr ...)))
          (=> unmatch)
          (cset-combine
           (filter 
            values ;; only generate the successful csets
            (for*/list ([t-arr t-arr] [s-arr s-arr])
                       (with-handlers ([exn:infer? (lambda (_) #f)])
                         (match* (t-arr s-arr)
                                 [((arr: ts t t-rest t-thn-eff t-els-eff) (arr: ss s s-rest s-thn-eff s-els-eff))
                                  (let ([arg-mapping 
                                         (cond [(and t-rest s-rest (= (length ts) (length ss)))
                                                (cgen/list X V (cons s-rest ss) (cons t-rest ts))]
                                               [(and (not t-rest) (not s-rest) (= (length ts) (length ss)))
                                                (cgen/list X V ss ts)]
                                               [(and t-rest (not s-rest) (<= (length ts) (length ss)))
                                                (cgen/list X V ss (extend ss ts t-rest))]
                                               [(and s-rest (not t-rest) (>= (length ts) (length ss)))
                                                (cgen/list X V (extend ts ss s-rest) ts)]
                                               [else (fail! S T)])]
                                        [ret-mapping (cgen V X t s)])
                                    (cset-meet arg-mapping ret-mapping))])))))]
         [(_ _)
          (cond [(subtype S T) empty]
                ;; or, nothing worked, and we fail
                [else (fail! S T)])]))))

(define (subst-gen C R)
  (for/list ([(k v) (car (cset-maps C))])
    (match v
      [(struct c (S X T))
       (let ([var (hash-ref (free-vars* R) X Constant)])
         ;(printf "variance was: ~a~nR was ~a~n" var R)
         (list
          X
          (evcase var 
                 [Constant S]
                 [Covariant S]
                 [Contravariant T]
                 [Invariant 
                  #; ; don't fail, we just pretend in covariance
                  (unless (type-equal? S T)
                              ;(printf "invariant and not equal ~a ~a" S T)
                              (fail! S T))
                  S])))])))

(define (cgen/list X V S T)
  (cset-meet* X (for/list ([s S] [t T]) (cgen V X s t))))

;; X : variables to infer
;; S : actual argument types
;; T : formal argument types
;; R : result type or #f
;; returns a substitution
;; if R is #f, we don't care about the substituion
;; just return a boolean result
(define (infer X S T R [expected #f])
  (with-handlers ([exn:infer? (lambda _ #f)])    
    (let ([cs (cgen/list X null S T)])
      (if (not expected)
          (subst-gen cs R)
          (cset-meet cs (cgen null X R expected))))))

;; like infer, but T-var is the vararg type:
(define (infer/vararg X S T T-var R [expected #f])
  (define new-T (extend S T T-var))
  (and ((length S) . >= . (length T))
       (infer X S new-T R expected)))

;; Listof[A] Listof[B] B -> Listof[B]
;; pads out t to be as long as s
(define (extend s t extra)
  (append t (build-list (- (length s) (length t)) (lambda _ extra))))

(define (infer/simple S T R)
  (infer (fv/list T) S T R))


(define (i s t r)
  (infer/simple (list s) (list t) r))

;; this is *definitely* not yet correct

;; NEW IMPL
;; restrict t1 to be a subtype of t2
(define (restrict t1 t2)     
  ;; we don't use union map directly, since that might produce too many elements
  (define (union-map f l)
    (match l
      [(Union: es) 
       (let ([l (map f es)])
         ;(printf "l is ~a~n" l)
         (apply Un l))]))
  (cond
    [(subtype t1 t2) t1] ;; already a subtype          
    [(match t2
       [(Poly: vars t)
        (let ([subst (infer vars (list t1) (list t) t1)])
          (and subst (restrict t1 (subst-all subst t1))))]
       [_ #f])]           
    [(Union? t1) (union-map (lambda (e) (restrict e t2)) t1)]
    [(Mu? t1)
     (restrict (unfold t1) t2)]
    [(Mu? t2) (restrict t1 (unfold t2))]
    [(subtype t2 t1) t2] ;; we don't actually want this - want something that's a part of t1
    [(not (overlap t1 t2)) (Un)] ;; there's no overlap, so the restriction is empty
    [else t2] ;; t2 and t1 have a complex relationship, so we punt
    ))

;(trace infer cgen cset-meet* subst-gen)
;(trace cgen/list)

;(trace infer subst-gen cgen)
