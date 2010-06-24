#lang scheme
(require "../ast.ss"
         "env.ss")

(define (subst-term env t)
  (match t
    [(struct variable (_ var))
     (lookup env var t)]
    [_
     t]))

(define (subst-literal env lit)
  (make-literal (literal-srcloc lit)
                (literal-predicate lit)
                (map (lambda (t) (subst-term env t))
                     (literal-terms lit))))

(define (subst-clause env c)
  (make-clause (clause-srcloc c)
               (subst-literal env (clause-head c))
               (map (lambda (l) (subst-literal env l))
                    (clause-body c))))

(define (shuffle env lit)
  (match lit
    [(struct literal (_ pred terms))
     (let loop ([env env]
                [terms terms])
       (match terms
         [(list) 
          env]
         [(list-rest (struct constant (_ value)) terms)
          (loop env terms)]
         [(list-rest (struct variable (srcloc var)) terms)
          (if (lookup env var)
              (loop env terms)
              (loop (extend env var (make-variable srcloc (gensym var))) terms))]))]))

(define (rename-clause c)
  (define env
    (foldl (lambda (e a)
             (shuffle a e))
           (shuffle (empty-env) (clause-head c))
           (clause-body c)))  
  (subst-clause env c))  

(define (rename-literal lit)
  (subst-literal (shuffle (empty-env) lit) lit))

(provide/contract
 [subst-term (env/c term/c . -> . term/c)]
 [subst-clause (env/c clause? . -> . clause?)]
 [rename-clause (clause? . -> . clause?)]
 [rename-literal (literal? . -> . literal?)])