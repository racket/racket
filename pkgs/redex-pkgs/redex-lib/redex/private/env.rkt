#lang typed/racket

(provide (struct-out env)
         empty-env
         add-name
         pure-nrep
         env-union
         (struct-out t-env)
         t-env-name-ref
         t-env-nrep-ref)

;; For now, accept any pattern
(define-type Pattern Any)
(define-type Term Any)
(define-type Env env)
(define-type TEnv t-env)
(define-type Tag Integer)
(define-type (Tagged a) (HashTable Tag a))

(struct: env ([names : (HashTable Symbol Pattern)]
              [nreps : (HashTable Symbol (Pairof Env (Tagged Pattern)))])
         #:transparent)

(struct: t-env ([names : (HashTable Symbol Term)]
                [nreps : (HashTable Symbol (Listof (Pairof TEnv (Tagged Term))))])
         #:transparent)

(: empty-env : Env)
(define empty-env
  (env (hash) (hash)))

(: add-name : Env Symbol Pattern -> Env)
(define/match (add-name e n p)
  [((env names nreps) _ _)
   (define (default) p)
   (define update identity)
   (env (hash-update names n update default) nreps)])

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
  (env (hash)
       nreps))

(: t-env-name-ref : TEnv Symbol -> Pattern)
(define/match (t-env-name-ref e n)
  [((t-env names _) _)
   (hash-ref names n (thunk (error (format "t-env-name-ref: name not found: ~s" n))))])

(: t-env-nrep-ref : TEnv Symbol -> (Listof (Pairof TEnv Term)))
(define/match (t-env-nrep-ref nv n)
  [((t-env _ nreps) n)
   (hash-ref nreps n (thunk (error (format "t-env-nrep-ref: repeat not found: ~s" n))))])

(: env-union : Env Env -> Env)
(define/match (env-union e1 e2)
  [((env ns1 rs1) (env ns2 rs2))
   
   (define names-union
     (hash-union ns1
                 ns2
                 (λ (_ v1 v2) v1)))
   (: combo : Symbol (Pairof Env (Tagged Pattern)) (Pairof Env (Tagged Pattern)) -> (Pairof Env (Tagged Pattern)))
   (define/match (combo _ e-t1 e-t2)
     [(_ (cons nv1 t1) (cons nv2 t2))
      (cons (env-union nv1 nv2)
            (hash-union t1 t2 (λ (t _1 _2) (error (format "2 tags should never collide, but these did: ~s, ~s with tag: ~s in envs ~s and ~s" _1 _2 t e1 e2)))))])
   (define nreps-union
     (hash-union rs1 rs2 combo))
   (env names-union nreps-union)])

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
            [else (or v1 v2 (error "absurd"))]))
    (values k v)))
