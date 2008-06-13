#lang scheme/unit

(require "type-effect-convenience.ss" "type-rep.ss" "effect-rep.ss" "rep-utils.ss"
         "free-variance.ss" "type-utils.ss" "union.ss" "tc-utils.ss" "type-name-env.ss"
         "subtype.ss" "remove-intersect.ss" "signatures.ss" "utils.ss"
         "constraint-structs.ss"
         scheme/match
         mzlib/etc
         mzlib/trace
         scheme/list)

(import dmap^ constraints^ promote-demote^)
(export infer^)

(define (empty-set) '())  

(define current-seen (make-parameter (empty-set)))

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

(define (move-rest-to-dmap cset dbound)
  (mover cset dbound (list dbound)
         (lambda (cmap)
           (make-dcon null
                      (hash-ref cmap dbound
                                (lambda () (int-err "No constraint for bound ~a" dbound)))))))

(define (move-vars+rest-to-dmap cset dbound vars)
  (mover cset dbound (list dbound)
         (lambda (cmap)
           (make-dcon (for/list ([v vars])
                        (hash-ref cmap v 
                                  (lambda () (int-err "No constraint for new var ~a" v))))
                      (hash-ref cmap dbound
                                (lambda () (int-err "No constraint for bound ~a" dbound)))))))
                                       

;; ss and ts have the same length
(define (cgen-union V X ss ts)
  ;; first, we remove common elements of ss and ts
  (let-values ([(ss* ts*)
                (values (filter (lambda (se) (not (ormap (lambda (t) (type-equal? t se)) ts))) ss)
                        (filter (lambda (te) (not (ormap (lambda (s) (type-equal? s te)) ss))) ts))])
    (cgen/list V X ss* ts*)))

(define (cgen/arr V X t-arr s-arr)
  (define (cg S T) (cgen V X S T))
  (match* (t-arr s-arr)
          [((arr: ts t #f #f t-thn-eff t-els-eff)
            (arr: ss s #f #f s-thn-eff s-els-eff))
           (cset-meet (cgen/list X V ss ts)
                      (cg t s))]
          [((arr: ts t t-rest #f t-thn-eff t-els-eff)
            (arr: ss s s-rest #f s-thn-eff s-els-eff))
           (let ([arg-mapping 
                  (cond [(and t-rest s-rest (<= (length ts) (length ss)))
                         (cgen/list X V (cons s-rest ss) (cons t-rest (extend ss ts t-rest)))]
                        [(and t-rest s-rest (>= (length ts) (length ss)))
                         (cgen/list X V (cons s-rest (extend ts ss s-rest)) (cons t-rest ts))]
                        [(and t-rest (not s-rest) (<= (length ts) (length ss)))
                         (cgen/list X V ss (extend ss ts t-rest))]
                        [(and s-rest (not t-rest) (>= (length ts) (length ss)))
                         (cgen/list X V (extend ts ss s-rest) ts)]
                        [else (fail! S T)])]
                 [ret-mapping (cg t s)])
             (cset-meet arg-mapping ret-mapping))]
          [((arr: ts t #f (cons dty dbound) t-thn-eff t-els-eff)
            (arr: ss s #f #f                s-thn-eff s-els-eff))
           (unless (memq dbound X)
             (fail! S T))
           (unless (<= (length ts) (length ss))
             (fail! S T))
           (let* ([num-vars (- (length ss) (length ts))]
                  [vars     (for/list ([n (in-range num-vars)])
                              (gensym dbound))]
                  [new-tys  (for/list ([var vars])
                              (substitute (make-F var) dbound dty))]
                  [new-cset (cgen/arr V (append vars X) (make-arr (append ts new-tys) t #f #f t-thn-eff t-els-eff) s-arr)])
             (move-vars-to-dmap new-cset dbound vars))]
          [((arr: ts t #f #f                t-thn-eff t-els-eff)
            (arr: ss s #f (cons dty dbound) s-thn-eff s-els-eff))
           (unless (memq dbound X)
             (fail! S T))
           (unless (<= (length ss) (length ts))
             (fail! S T))
           (let* ([num-vars (- (length ts) (length ss))]
                  [vars     (for/list ([n (in-range num-vars)])
                              (gensym dbound))]
                  [new-tys  (for/list ([var vars])
                              (substitute (make-F var) dbound dty))]
                  [new-cset (cgen/arr V (append vars X) t-arr (make-arr (append ss new-tys) s #f #f s-thn-eff s-els-eff))])
             (move-vars-to-dmap new-cset dbound vars))]
          [((arr: ts t #f (cons t-dty dbound) t-thn-eff t-els-eff)
            (arr: ss s #f (cons s-dty dbound) s-thn-eff s-els-eff))
           (unless (= (length ts) (length ss))
             (fail! S T))
           ;; If we want to infer the dotted bound, then why is it in both types?
           (when (memq dbound X)
             (fail! S T))
           (let* ([arg-mapping (cgen/list X V ss ts)]
                  [darg-mapping (cgen (cons dbound V) X s-dty t-dty)]
                  [ret-mapping (cg t s)])
             (cset-meet* (list arg-mapping darg-mapping ret-mapping)))]
          [((arr: ts t t-rest #f                  t-thn-eff t-els-eff)
            (arr: ss s #f     (cons s-dty dbound) s-thn-eff s-els-eff))
           (unless (memq dbound X)
             (fail! S T))
           (if (<= (length ts) (length ss))
               ;; the simple case
               (let* ([arg-mapping (cgen/list X V ss (extend ss ts t-rest))]
                      [darg-mapping (move-rest-to-dmap (cgen (cons dbound V) X s-dty t-rest) dbound)]
                      [ret-mapping (cg t s)])
                 (cset-meet* (list arg-mapping darg-mapping ret-mapping)))
               ;; the hard case
               (let* ([num-vars (- (length ts) (length ss))]
                      [vars     (for/list ([n (in-range num-vars)])
                                  (gensym dbound))]
                      [new-tys  (for/list ([var vars])
                                  (substitute (make-F var) dbound s-dty))]
                      [new-cset (cgen/arr V (append vars X) t-arr 
                                          (make-arr (append ss new-tys) s #f (cons s-dty dbound) s-thn-eff s-els-eff))])
                 (move-vars+rest-to-dmap new-cset dbound vars)))]
          ;; If dotted <: starred is correct, add it below.  Not sure it is.
          [(_ _) (fail! S T)]))
    
(define (cgen V X S T) 
  (define (cg S T) (cgen V X S T))
  (define empty (empty-cset X))
  (define (singleton S X T)
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
          (cg b b*)]
         
         
         [((Union: es) S) (cset-meet* (for/list ([e es]) (cg e S)))]
         ;; we might want to use multiple csets here, but I don't think it makes a difference
         [(S (Union: es)) (or
                           (for/or 
                            ([e es]) 
                            (with-handlers
                                ([exn:infer? (lambda _ #f)])
                              (cg S e)))
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
          (cset-meet (cg a a*) (cg b b*))]
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
          (let ([x (instantiate-poly (lookup-type-name n) args)]
                [y (instantiate-poly (lookup-type-name n) args*)])
            (cg x y))]
         [((Vector: e) (Vector: e*))
          (cset-meet (cg e e*) (cg e* e))]
         [((Box: e) (Box: e*))
          (cset-meet (cg e e*) (cg e* e))]
         [((Hashtable: s1 s2) (Hashtable: t1 t2))
          ;; the key is covariant, the value is invariant
          (cset-meet* (list (cg s1 t1) (cg t2 s2) (cg s2 t2)))]
         [((Syntax: s1) (Syntax: s2))
          (cg s1 s2)]
         ;; parameters are just like one-arg functions
         [((Param: in1 out1) (Param: in2 out2))
          (cset-meet (cg in2 in1) (cg out1 out2))]
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
         [(_ _)
          (cond [(subtype S T) empty]
                ;; or, nothing worked, and we fail
                [else (fail! S T)])]))))

(define (subst-gen C R)
  (for/list ([(k v) (car (car (cset-maps C)))])
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
  (cset-meet* (for/list ([s S] [t T]) (cgen V X s t))))

;; X : variables to infer
;; S : actual argument types
;; T : formal argument types
;; R : result type
;; expected : boolean
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

;; like infer, but dotted-var is the bound on the ...
;; and T-dotted is the repeated type
(define (infer/dots X dotted-var S T T-dotted R [expected #f])
  (with-handlers ([exn:infer? (lambda _ #f)])    
    (let* ([short-S (take S (length T))]
           [rest-S (drop S (length T))]
           [cs-short (cgen/list (cons dotted-var X) null short-S T)]
           [new-vars (for/list ([i (in-range (length rest-S))]) (gensym dotted-var))]
           [new-Ts (for/list ([v new-vars])
                     (substitute (make-F v) dotted-var T-dotted))]
           [cs-dotted (cgen/list (append new-vars X) null rest-S new-Ts)]
           [cs-dotted* (move-vars-to-dmap cs-dotted dotted-var new-vars)]
           [cs (cset-meet cs-short cs-dotted*)])
      (if (not expected)
          (subst-gen cs R)
          (cset-meet cs (cgen null X R expected))))))

(define (infer/simple S T R)
  (infer (fv/list T) S T R))

(define (i s t r)
  (infer/simple (list s) (list t) r))
