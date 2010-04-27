#lang scheme/base  

(require scheme/contract
         (prefix-in r: "../utils/utils.ss")
         scheme/match (r:rep filter-rep rep-utils) unstable/struct
         (except-in (r:utils tc-utils) make-env)
         (r:typecheck tc-metafunctions))

(provide current-tvars
         extend
         env?
         lookup
         extend-env
         extend/values
         dotted-env
         initial-tvar-env
         env-map
         env-filter
         env-vals
         env-keys+vals
         env-props
         replace-props
         with-dotted-env/extend)

;; eq? has the type of equal?, and l is an alist (with conses!)
;; props is a list of known propositions
(r:d-s/c env ([eq? (any/c any/c . -> . boolean?)] [l (listof pair?)] [props (listof Filter/c)]) #:transparent)

(define (env-vals e)
  (map cdr (env-l e)))

(define (env-keys+vals e)
  (env-l e))

(define (env-filter f e)
  (match e
    [(struct env (eq? l props))
    (make-env eq? (filter f l) props)]))

(define (make-empty-env p?) (make env p? null null))

;; the initial type variable environment - empty
;; this is used in the parsing of types
(define initial-tvar-env (make-empty-env eq?))

;; a parameter for the current type variables
(define current-tvars (make-parameter initial-tvar-env))  

;; the environment for types of ... variables
(define dotted-env (make-parameter (make-empty-env free-identifier=?)))

(define/contract (env-map f e)
  ((pair? . -> . pair?) env? . -> . env?)
  (make env (env-eq? e) (map f (env-l e)) (env-props e)))

;; extend that works on single arguments
(define (extend e k v) 
  (match e
    [(struct env (f l p)) (make env f (cons (cons k v) l) p)]
    [_ (int-err "extend: expected environment, got ~a" e)]))

(define (extend-env ks vs e)
  (match e
    [(struct env (f l p)) (make env f (append (map cons ks vs) l) p)]
    [_ (int-err "extend-env: expected environment, got ~a" e)]))

(define (replace-props e props)
  (match e
    [(struct env (f l p))
     (make env f l props)]))

(define (lookup e key fail)
  (match e
    [(struct env (f? l p))
     (let loop ([l l])
       (cond [(null? l) (fail key)]
             [(f? (caar l) key) (cdar l)]
             [else (loop (cdr l))]))]
    [_ (int-err "lookup: expected environment, got ~a" e)]))


;; takes two lists of sets to be added, which are either added one at a time, if the 
;; elements are not lists, or all at once, if the elements are lists
(define (extend/values kss vss env)
  (foldr (lambda (ks vs env) 
           (cond [(and (list? ks) (list? vs))
                  (extend-env ks vs env)]
                 [(or (list? ks) (list? vs))
                  (int-err "not both lists in extend/values: ~a ~a" ks vs)]
                 [else (extend-env (list ks) (list vs) env)]))
         env kss vss))

;; run code in an extended dotted env
(define-syntax with-dotted-env/extend
  (syntax-rules ()
    [(_ i t v . b) (parameterize ([dotted-env (extend/values (list i) (list (cons t v)) (dotted-env))]) . b)]))

(provide/contract [make-empty-env ((-> any/c any/c any/c) . -> . env?)])
