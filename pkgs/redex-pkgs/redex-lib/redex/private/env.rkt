#lang typed/racket

(provide env
         env?
         empty-env
         add-name
         env-name-ref
         env-union
         env-names)

;; For now, accept any pattern
(define-type Pattern Any)
(define-type Tag Number)
(define-type Env env)

(struct: env ([names : (HashTable Symbol Pattern)])
         #:transparent)

(: empty-env : Env)
(define empty-env
  (env (hash)))

(: add-name : Env Symbol Pattern -> Env)
(define/match (add-name e n p)
  [((env names) _ _)
   (define (default) p)
   (define update identity)
   (env (hash-update names n update default))])

(: env-name-ref : Env Symbol -> Pattern)
(define/match (env-name-ref e n)
  [((env names) _)
   (hash-ref names n (λ () (error (format "env-name-ref: name not found: ~s" n))))])

(: env-union : Env Env -> Env)
(define/match (env-union e1 e2)
  [((env ns1) (env ns2))
   (define ks1 (list->set (hash-keys ns1)))
   (define ks2 (list->set (hash-keys ns2)))
   ;; For some reason in-set is not typed so I can't use it
   (env (for/hash: : (HashTable Symbol Pattern)
                     ([k : Symbol (set-union ks1 ks2)])
          (: ref-default : (HashTable Symbol Pattern) -> Pattern)
          (define (ref-default ns)
            (hash-ref ns k (thunk #f)))
          (match-define p1 (ref-default ns1))
          (match-define p2 (ref-default ns2))
          (values k (or p1 p2))))])
