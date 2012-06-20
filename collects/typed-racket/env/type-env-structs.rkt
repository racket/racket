#lang racket/base

(require racket/contract unstable/sequence racket/dict syntax/id-table
         (prefix-in r: "../utils/utils.rkt")
         racket/match (r:rep filter-rep rep-utils type-rep) unstable/struct
         (except-in (r:utils tc-utils) make-env))

(provide extend
         env?
         lookup
         extend-env
         extend/values
         env-map
         make-empty-env
         env-filter
         env-keys+vals
         env-props
         replace-props
         prop-env? make-empty-prop-env)

;; eq? has the type of equal?, and l is an alist (with conses!)
;; props is a list of known propositions
(r:define-struct/cond-contract env ([l (and/c (not/c dict-mutable?) dict?)])
         #:transparent
         #:property prop:custom-write
         (lambda (e prt mode)
           (fprintf prt "(env ~a)" (dict-map (env-l e) list))))
(r:define-struct/cond-contract (prop-env env) ([props (listof Filter/c)])
         #:transparent
         #:property prop:custom-write
         (lambda (e prt mode)
           (fprintf prt "(env ~a ~a)" (dict-map (env-l e) list) (prop-env-props e))))

(define (mk-env orig dict)
  (match orig
    [(prop-env _ p) (prop-env dict p)]
    [_ (env dict)]))

(define (env-filter f e)
  (match e
    [(env l)
     (mk-env e
             (for/fold ([h l])
                       ([(k v) (in-dict l)]
                        #:unless (f (cons k v)))
               (dict-remove h k)))]))

(r:define/cond-contract (make-empty-env dict)
       (dict? . -> . env?)
       (env dict))

(r:define/cond-contract (make-empty-prop-env dict)
       (dict? . -> . prop-env?)
       (prop-env dict null))

(r:define/cond-contract (env-props e)
       (prop-env? . -> . (listof Filter/c))
       (prop-env-props e))

(define (env-keys+vals e)
  (match e
    [(env l) (for/list ([(k v) (in-dict l)]) (cons k v))]))

(r:define/cond-contract (env-map f e)
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
    [(prop-env l p)
     (prop-env l props)]))

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

