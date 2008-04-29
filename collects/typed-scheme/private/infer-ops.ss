#lang scheme/base

(require "type-effect-convenience.ss" "type-rep.ss" "effect-rep.ss" "rep-utils.ss"
         "free-variance.ss" "type-utils.ss" "union.ss" "tc-utils.ss"
         "remove-intersect.ss" "subtype.ss"
         scheme/match
         mzlib/etc
         mzlib/trace
         scheme/list)

(provide (all-defined-out))

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
             [#:Vector t (inv t)]
             [#:Box t (inv t)]
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
             [#:Vector t (inv t)]
             [#:Box t (inv t)]
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
(define-struct cset (map) #:prefab)

(define (empty-cset X)
  (make-cset (for/hash ([x X]) (values x (make-c (Un) x Univ)))))

(define (lookup cset var)
  (hash-ref (cset-map cset) var (make-c (Un) var Univ)))

(define (insert cs var S T)
  (match cs
    [(struct cset (map))
     (make-cset (hash-set map var (make-c S var T)))]))

(define c-meet
  (match-lambda**
   [((struct c (S X T)) (struct c (S* _ T*)))
    (make-c (join S S*) X (meet T T*))]))
    

(define cset-meet
  (match-lambda**
   [((struct cset (map1)) (struct cset (map2)))
    (make-cset (for/hash ([(k v1) map1])                         
                         (values k (c-meet v1 (hash-ref map2 k)))))]))

(define (cset-meet* V args)
  (for/fold ([c (empty-cset V)])
    ([a args])
    (cset-meet a c)))




(define (cgen V X S T)
  (define empty (empty-cset X))
  (define (singleton S X T )
    (insert empty X S T))
  (parameterize ([match-equality-test type-equal?])
    (match* 
     (S T)
     [(a a) empty]
     [(_ (Univ:)) empty]
     [((Union: es) S) (cset-meet* X (for/list ([e es]) (cgen V X e S)))]
     [(S (Union: es)) (or
                       (for/or 
                        ([e es]) 
                        (with-handlers
                            ([exn:infer? (lambda _ #f)])
                          (cgen V X S e)))
                       (fail! S T))]
     [((F: (? (lambda (e) (memq e X)) v)) S)
      (singleton (Un) v (var-demote S V))]
     [(S (F: (? (lambda (e) (memq e X)) v)))
      (singleton (var-promote S V) v Univ)]
     [((Struct: nm p flds proc) (Struct: nm p flds* proc*))
      (let-values ([(flds flds*)
                    (cond [(and proc proc*)
                           (values (cons proc flds) (cons proc* flds*))]
                          [(or proc proc*)
                           (fail! S T)]
                          [else (values flds flds*)])])
        (cset-meet* X (for/list ([f flds] [f* flds*])
                        (cgen V X f f*))))]
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
      (cset-meet* X (for/list ([a args] [a* args*]) (cgen V X a a*)))]
     [((Vector: e) (Vector: e*))
      (cset-meet (cgen V X e e*) (cgen V X e* e))]
     [((Function: (list t-arr ...))
       (Function: (list s-arr ...)))
      (=> unmatch)
      (let loop ([t-arr t-arr] [s-arr s-arr] [cset empty])
        (cond [(and (null? t-arr) (null? s-arr)) cset]
              [(or (null? t-arr) (null? s-arr)) (unmatch)]
              [else (match (list (car t-arr) (car s-arr))
                      [(list (arr: ts t t-rest t-thn-eff t-els-eff) (arr: ss s s-rest s-thn-eff s-els-eff))
                       (let ([arg-mapping 
                              (cond [(and t-rest s-rest (= (length ts) (length ss)))
                                     (cset-meet* X (for/list ([t (cons t-rest ts)] [s (cons s-rest ss)])
                                                     (cgen V X s t)))]
                                    [(and (not t-rest) (not s-rest) (= (length ts) (length ss)))
                                     (cset-meet* X (for/list ([t ts] [s ss])
                                                     (cgen V X s t)))]
                                    [(and t-rest (not s-rest) (<= (length ts) (length ss)))
                                     (cset-meet* X (for/list ([s ss] [t (extend ss ts t-rest)])
                                                             (cgen V X s t)))]
                                    [(and s-rest (not t-rest) (>= (length ts) (length ss)))
                                     (cset-meet* X (for/list ([s (extend ts ss s-rest)] [t ts])
                                                             (cgen V X s t)))]
                                    [else (unmatch)])]
                             [ret-mapping (cgen V X t s)])
                         (loop (cdr t-arr) (cdr s-arr)
                               (cset-meet* X (list cset arg-mapping ret-mapping))))])]))]
     [(_ _)
      (cond [(subtype S T) empty]
            ;; or, nothing worked, and we fail
            [else (fail! S T)])])))

(define (subst-gen C R)
  (for/list ([(k v) (cset-map C)])
    (match v
      [(struct c (S X T))
       (let ([var (hash-ref (free-vars* R) X Constant)])
         (list
          X
          (evcase var 
                 [Constant S]
                 [Covariant S]
                 [Contravariant T]
                 [Invariant (unless (type-equal? S T)
                              (fail! S T))
                            S])))])))
;; X : variables to infer
;; S : actual argument types
;; T : formal argument types
;; R : result type or #f
;; returns a substitution
;; if R is #f, we don't care about the substituion
;; just return a boolean result
(define (infer X S T R)
  (with-handlers ([exn:infer? (lambda _ #f)])
    (let ([cs (cset-meet* X (for/list ([s S] [t T]) (cgen null X s t)))])
    (if R
        (subst-gen cs R)
        #t))))

;; like infer, but T-var is the vararg type:
(define (infer/vararg X S T T-var R)
  (define new-T (extend S T T-var))
  (and ((length S) . >= . (length T))
       (infer X S new-T R)))

;; Listof[A] Listof[B] B -> Listof[B]
;; pads out t to be as long as s
(define (extend s t extra)
  (append t (build-list (- (length s) (length t)) (lambda _ extra))))

(define (infer/simple S T R)
  (infer (fv/list T) S T R))

;(trace infer #;cgen #;cset-meet*)

;(trace cgen)

(define (i s t r)
  (infer/simple (list s) (list t) r))
