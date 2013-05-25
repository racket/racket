#lang racket/base
(require racket/match
         racket/function
         racket/contract
         "../ast.rkt"
         "env.rkt")

(define (subst-term env t)
  (match t
    [(struct variable (_ var))
     (lookup env var t)]
    [_
     t]))

(define (subst-terms env ts)
  (map (curry subst-term env) ts))

(define (subst-literal env lit)
  (struct-copy 
   literal lit
   [terms 
    (subst-terms env (literal-terms lit))]))

(define (subst-external env ext)
  (struct-copy 
   external ext
   [arg-terms 
    (subst-terms env (external-arg-terms ext))]
   [ans-terms 
    (subst-terms env (external-ans-terms ext))]))

(define (subst-question env q)
  (match q
    [(? literal?) (subst-literal env q)]
    [(? external?) (subst-external env q)]))

(define (subst-clause env c)
  (clause (clause-srcloc c)
          (subst-literal env (clause-head c))
          (map (curry subst-question env)
               (clause-body c))))

(define (shuffle-terms env terms)
  (match terms
    [(list) 
     env]
    [(list-rest (constant _ value) terms)
     (shuffle-terms env terms)]
    [(list-rest (variable srcloc var) terms)
     (if (lookup env var)
         (shuffle-terms env terms)
         (shuffle-terms (extend env var (make-variable srcloc (gensym var)))
                        terms))]))

(define (shuffle env q)
  (match q
    [(external _ _ pred arg-terms ans-terms)
     (shuffle-terms env (append arg-terms ans-terms))]
    [(literal _ pred terms)
     (shuffle-terms env terms)]))

(define (rename-clause c)
  (define env
    (foldl (lambda (e a)
             (shuffle a e))
           (shuffle (empty-env) (clause-head c))
           (clause-body c)))  
  (subst-clause env c))  

(define (rename-question q)
  (subst-question (shuffle (empty-env) q) q))

(provide/contract
 [subst-terms (env/c (listof term/c) . -> . (listof term/c))]
 [subst-term (env/c term/c . -> . term/c)]
 [subst-clause (env/c clause? . -> . clause?)]
 [rename-clause (clause? . -> . clause?)]
 [rename-question (question/c . -> . question/c)])
