#lang typed/racket

(require/typed "error.rkt"
               [redex-error (Symbol String Any * -> Nothing)])
(require racket/set)

(provide (struct-out env)
         empty-env
         add-name
         add-mismatch
         pure-nrep
         env-union
         (struct-out t-env)
         t-env-name-ref
         t-env-misname-ref
         t-env-nrep-ref)

;; For now, accept any pattern
(define-type Pattern Any)
(define-type Term Any)
(define-type Env env)
(define-type TEnv t-env)
(define-type Tag Integer)
(define-type (Tagged a) (HashTable Tag a))

(struct: env ([names    : (HashTable Symbol Pattern)]
              [misnames : (HashTable Symbol (Pairof Pattern (Setof Tag)))]
              [nreps    : (HashTable Symbol (Pairof Env (Tagged Pattern)))])
         #:transparent)

(struct: t-env ([names    : (HashTable Symbol Term)]
                [misnames : (HashTable Symbol (Listof (Pairof Tag Term)))]
                [nreps    : (HashTable Symbol (Listof (Pairof TEnv (Tagged Term))))])
         #:transparent)

(: empty-env : Env)
(define empty-env
  (env (hash) (hash) (hash)))

(: empty-t-env : TEnv)
(define empty-t-env
  (t-env (hash) (hash) (hash)))

(: add-name : Env Symbol Pattern -> Env)
(define/match (add-name e n p)
  [((env names misnames nreps) _ _)
   (define (default) p)
   (define update identity)
   (env (hash-update names n update default) misnames nreps)])

(: add-mismatch : Env Symbol Pattern Tag -> Env)
(define/match (add-mismatch e n p t)
  [((env names misnames nreps) _ _ _)
   (: default : -> (Pairof Pattern (Setof Tag)))
   (define (default) (cons p (set t)))
   (: update : (Pairof Pattern (Setof Tag)) -> (Pairof Pattern (Setof Tag)))
   (define/match (update p-ts)
     [((cons p ts))
      (cons p (set-add ts t))])
   (env names
        (hash-update misnames n update default)
        nreps)])

(: pure-nrep : Symbol Env Tag Pattern -> Env)
(define (pure-nrep n repnv tag pat)
  (: nreps : (HashTable Symbol (Pairof Env (Tagged Pattern))))
  (define nreps
    (hash-set (ann (hash) (HashTable Symbol (Pairof Env (Tagged Pattern))))
              n
              (cons repnv
                    (hash-set (ann (hash) (Tagged Pattern))
                              tag
                              pat))))
  (env (hash) (hash) nreps))

(: t-env-name-ref : TEnv Symbol -> Term)
(define/match (t-env-name-ref e n)
  [((t-env names _ _) _)
   (hash-ref names n (thunk (redex-error 't-env-name-ref "name not found: ~s" n)))])

(: t-env-misname-ref : TEnv Symbol Tag -> Term)
(define/match (t-env-misname-ref te m tag)
  [((t-env _ misnames _) _ _)
   (define tagged-terms
     (hash-ref misnames m (thunk (redex-error 't-env-misname-ref "mismatch name not found: ~s" m))))
   (define maybe-term
     (assoc tag tagged-terms))
   (cond [maybe-term (cdr maybe-term)]
         [else (redex-error 't-env-misname-ref "mismatch name tag not found: ~s" tag)])])

(: t-env-nrep-ref : TEnv Symbol -> (Listof (Pairof TEnv Term)))
(define/match (t-env-nrep-ref nv n)
  [((t-env _ _ nreps) n)
   (hash-ref nreps n (thunk (redex-error 't-env-nrep-ref "repeat not found: ~s" n)))])

(: env-union : Env Env -> Env)
(define/match (env-union e1 e2)
  [((env ns1 ms1 rs1) (env ns2 ms2 rs2))
   
   (define names-union
     (hash-union ns1
                 ns2
                 (λ (_ v1 v2)
                    (unless (equal? v1 v2)
                      (redex-error 'generate-term-#:ith "named patterns must be the same pattern"))
                    v1)))

   (: mis-combo : Symbol (Pairof Pattern (Setof Tag)) (Pairof Pattern (Setof Tag)) -> (Pairof Pattern (Setof Tag)))
   (define/match (mis-combo k pts1 pts2)
     [(_ (cons p1 ts1) (cons p2 ts2))
      (unless (equal? p1 p2)
        (redex-error 'generate-term-#:ith "mismatch named patterns must be the same pattern"))
      (cons p1 (set-union ts1 ts2))])
   
   (: misnames-union : (HashTable Symbol (Pairof Pattern (Setof Tag))))
   (define misnames-union
     (hash-union ms1 ms2 mis-combo))
   
   (: nrep-combo : Symbol (Pairof Env (Tagged Pattern)) (Pairof Env (Tagged Pattern)) -> (Pairof Env (Tagged Pattern)))
   (define/match (nrep-combo _ e-t1 e-t2)
     [(_ (cons nv1 t1) (cons nv2 t2))
      (cons (env-union nv1 nv2)
            (hash-union t1 t2
                        (λ (t _1 _2)
                           (redex-error 'env-union
                                        "2 tags should never collide, but these did: ~s, ~s with tag: ~s in envs ~s and ~s"
                                        _1 _2 t e1 e2))))])
   (define nreps-union
     (hash-union rs1 rs2 nrep-combo))
   (env names-union misnames-union nreps-union)])

(: key-set : (All (k v) (HashTable k v) -> (Setof k)))
(define (key-set m)
  (list->set (hash-keys m)))

(: hash-union : (All (k v) (HashTable k v) (HashTable k v) (k v v -> v) -> (HashTable k v)))
(define (hash-union m1 m2 combo)
  (: ks1 : (Setof k))
  (: ks2 : (Setof k))
  (define ks1 (key-set m1))
  (define ks2 (key-set m2))
  (for/hash: : (HashTable k v)
               ([k : k (in-set (set-union ks1 ks2))])
    (define v1 (hash-ref m1 k (thunk #f)))
    (define v2 (hash-ref m2 k (thunk #f)))
    (define v
      (cond [(and v1 v2)
             (combo k v1 v2)]
            [else (or v1 v2 (redex-error 'absurd ""))]))
    (values k v)))
