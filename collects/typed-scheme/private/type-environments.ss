#lang scheme/base  

(provide current-tvars
         extend
         lookup
         make-empty-env
         extend-env
         extend/values
         initial-tvar-env)

(require scheme/match
         "tc-utils.ss")

;; eq? has the type of equal?, and l is an alist (with conses!)
(define-struct env (eq? l))

;; the initial type variable environment - empty
;; this is used in the parsing of types
(define initial-tvar-env (make-env eq? '()))

;; a parameter for the current type variables
(define current-tvars (make-parameter initial-tvar-env))  

(define (make-empty-env p?) (make-env p? '()))


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

