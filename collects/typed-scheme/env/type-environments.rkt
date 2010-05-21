#lang scheme/base  

(require scheme/contract unstable/sequence racket/dict syntax/id-table
         (prefix-in r: "../utils/utils.rkt")
         scheme/match (r:rep filter-rep rep-utils type-rep) unstable/struct
         (except-in (r:utils tc-utils) make-env)
         #;(r:typecheck tc-metafunctions))

(provide current-tvars
         extend
         env?
         lookup
         extend-env
         extend/values
         dotted-env
         initial-tvar-env
         env-map
         make-empty-env
         env-filter
         env-keys+vals
         env-props
         replace-props
         with-dotted-env/extend)

;; eq? has the type of equal?, and l is an alist (with conses!)
;; props is a list of known propositions
(r:d-s/c env ([l (and/c (not/c dict-mutable?) dict?)] [props (listof Filter/c)]) #:transparent)

(define (env-filter f e)
  (match e
    [(struct env (l props))
     (make-env (for/fold ([h l])
                 ([(k v) (in-dict l)]
                  #:when (not (f (cons k v))))
                 (dict-remove h k))
               props)]))

(r:d/c (make-empty-env dict)
       (dict? . -> . env?)
       (make-env dict null))

(define (env-keys+vals e)
  (match e
    [(env l _) (for/list ([(k v) (in-dict l)]) (cons k v))]))

;; the initial type variable environment - empty
;; this is used in the parsing of types
(define initial-tvar-env (make-empty-env #hasheq()))

;; a parameter for the current type variables
(define current-tvars (make-parameter initial-tvar-env))  

;; the environment for types of ... variables
(define dotted-env (make-parameter (make-empty-env (make-immutable-free-id-table))))

(r:d/c (env-map f e)
  ((any/c any/c . -> . any/c) env? . -> . env?)
  (make-env (dict-map f (env-l e)) (env-props e)))

;; extend that works on single arguments
(define (extend e k v) 
  (match e
    [(env l p) (make-env (dict-set l k v) p)]
    [_ (int-err "extend: expected environment, got ~a" e)]))

(define (extend-env ks vs e)
  (match e
    [(env l p) (make-env (for/fold ([h l])
                           ([k (in-list ks)] [v (in-list vs)]) (dict-set h k v))
                         p)]
    [_ (int-err "extend-env: expected environment, got ~a" e)]))

(define (replace-props e props)
  (match e
    [(env l p)
     (make-env l props)]))

(define (lookup e key fail)
  (match e
    [(env l p) (dict-ref l key (λ () (fail key)))]
    [_ (int-err "lookup: expected environment, got ~a" e)]))


;; takes two lists of sets to be added, which are either added one at a time, if the 
;; elements are not lists, or all at once, if the elements are lists
(define (extend/values kss vss env)
  (foldr (lambda (ks vs env) 
           (cond [(and (list? ks) (list? vs))
                  (extend-env ks vs env)]
                 [(or (list? ks) (list? vs))
                  (int-err "not both lists in extend/values: ~a ~a" ks vs)]
                 [else (extend env ks vs)]))
         env kss vss))

;; run code in an extended dotted env
(define-syntax with-dotted-env/extend
  (syntax-rules ()
    [(_ i t v . b) (parameterize ([dotted-env (extend (dotted-env) i (cons t v))]) . b)]))
