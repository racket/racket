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
         with-dotted-env/extend
         lex-env? make-empty-lex-env)

;; eq? has the type of equal?, and l is an alist (with conses!)
;; props is a list of known propositions
(r:d-s/c env ([l (and/c (not/c dict-mutable?) dict?)]) #:transparent)
(r:d-s/c (lex-env env) ([props (listof Filter/c)]) #:transparent)

(define (mk-env orig dict)
  (match orig
    [(lex-env _ p) (lex-env dict p)]
    [_ (env dict)]))

(define (env-filter f e)
  (match e
    [(env l)
     (mk-env e 
             (for/fold ([h l])
               ([(k v) (in-dict l)]
                #:when (not (f (cons k v))))
               (dict-remove h k)))]))

(r:d/c (make-empty-env dict)
       (dict? . -> . env?)
       (env dict))

(r:d/c (make-empty-lex-env dict)
       (dict? . -> . lex-env?)
       (lex-env dict null))

(r:d/c (env-props e)
       (lex-env? . -> . (listof Filter/c))
       (lex-env-props e))

(define (env-keys+vals e)
  (match e
    [(env l) (for/list ([(k v) (in-dict l)]) (cons k v))]))

;; the initial type variable environment - empty
;; this is used in the parsing of types
(define initial-tvar-env (make-empty-env #hasheq()))

;; a parameter for the current type variables
(define current-tvars (make-parameter initial-tvar-env))  

;; the environment for types of ... variables
(define dotted-env (make-parameter (make-empty-env (make-immutable-free-id-table))))

(r:d/c (env-map f e)
  ((any/c any/c . -> . any/c) env? . -> . env?)
  (mk-env e (dict-map f (env-l e))))

;; extend that works on single arguments
(define (extend e k v) 
  (match e
    [(env l) (mk-env e (dict-set l k v))]
    [_ (int-err "extend: expected environment, got ~a" e)]))

(define (extend-env ks vs e)
  (match e
    [(env l) (mk-env e (for/fold ([h l])
                         ([k (in-list ks)] [v (in-list vs)]) (dict-set h k v)))]
    [_ (int-err "extend-env: expected environment, got ~a" e)]))

(define (replace-props e props)
  (match e
    [(lex-env l p)
     (lex-env l props)]))

(define (lookup e key fail)
  (match e
    [(env l) (dict-ref l key (λ () (fail key)))]
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
