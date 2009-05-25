#lang scheme/base  

(require scheme/contract
         (prefix-in r: "../utils/utils.ss")
         scheme/match
         (except-in (r:utils tc-utils) make-env))

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
         with-dotted-env/extend)

;; eq? has the type of equal?, and l is an alist (with conses!)
(r:d-s/c env ([eq? (any/c any/c . -> . boolean?)] [l (listof pair?)]) #:transparent)

(define (env-vals e)
  (map cdr (env-l e)))

(define (env-keys+vals e)
  (env-l e))

(define (env-filter f e)
  (match e
    [(struct env (eq? l))
    (make-env eq? (filter f l))]))

;; the initial type variable environment - empty
;; this is used in the parsing of types
(define initial-tvar-env (make-env eq? '()))

;; a parameter for the current type variables
(define current-tvars (make-parameter initial-tvar-env))  

(define (make-empty-env p?) (make-env p? '()))

;; the environment for types of ... variables
(define dotted-env (make-parameter (make-empty-env free-identifier=?)))

(define/contract (env-map f env)
  ((pair? . -> . pair?) env? . -> . env?)
  (make-env (env-eq? env) (map f (env-l env))))

;; extend that works on single arguments
(define (extend e k v) 
  (match e
    [(struct env (f l)) (make-env f (cons (cons k v) l))]
    [_ (int-err "extend: expected environment, got ~a" e)]))

(define (extend-env ks vs e)
  (match e
    [(struct env (f l)) (make-env f (append (map cons ks vs) l))]
    [_ (int-err "extend-env: expected environment, got ~a" e)]))

(define (lookup e key fail)
  (match e
    [(struct env (f? l))
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
