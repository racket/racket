#lang scheme
(require "ast.ss"
         "private/env.ss"
         "private/subst.ss"
         "private/unify.ss"
         "private/variant.ss")

; A clause is safe if every variable in its head occurs in some literal in its body.
(define (safe-clause? c)
  (define head-vars (filter variable? (literal-terms (clause-head c))))
  (andmap (lambda (v)
            (ormap (lambda (l)
                     (ormap (lambda (t) (term-equal? t v))
                            (literal-terms l)))
                   (clause-body c)))
          head-vars))

(define theory/c (coerce-contract 'exec hash?))
(define immutable-theory/c (and/c hash? immutable?))
(define mutable-theory/c (and/c hash? (not/c immutable?)))
(define (literal-key l)
  (format "~a/~a" (literal-predicate l) (length (literal-terms l))))
(define (clause-key c)
  (literal-key (clause-head c)))

(define (make-immutable-theory)
  (make-immutable-hash empty))
(define (make-mutable-theory)
  (make-hash))

(define ((mk-assume hash-update) thy c)
  (hash-update
   thy (clause-key c)
   (lambda (clauses)
     (list* c clauses))
   empty))
(define ((mk-retract hash-update) thy rc)  
  (hash-update
   thy (clause-key rc)
   (lambda (clauses)
     (filter (lambda (c)
               (not (clause-equal? c rc)))
             clauses))
   empty))

(define assume (mk-assume hash-update))
(define retract (mk-retract hash-update))
(define assume! (mk-assume hash-update!))
(define retract! (mk-retract hash-update!))

(define (get thy lit)
  (hash-ref thy (literal-key lit) empty))

(define-struct subgoal 
  (literal 
   [facts #:mutable]
   [waiters #:mutable]))

(define (resolve c lit)
  (define body (clause-body c))
  (and (not (empty? body))
       (cond
         [(unify (first body) (rename-literal lit))
          => (lambda (env)
               (subst-clause env (make-clause (clause-srcloc c) (clause-head c) (rest body))))]
         [else #f])))

(define (prove thy lit)
  (define subgoals (make-literal-tbl))
  (define (fact! sg lit)
    (unless (mem-literal lit (subgoal-facts sg))
      (set-subgoal-facts! sg (list* lit (subgoal-facts sg)))
      (for-each (lambda (w)
                  (cond
                    [(resolve (cdr w) lit)
                     => (lambda (cs-p) (add-clause! (car w) cs-p))]))
                (subgoal-waiters sg))))
  (define (rule! sg1 c s)
    (define sg2 (literal-tbl-find subgoals s))
    (if sg2
        (begin
          (set-subgoal-waiters! sg2 (list* (cons sg1 c) (subgoal-waiters sg2)))
          (for-each (lambda (fact)
                      (cond
                        [(resolve c fact)
                         => (lambda (cs) (add-clause! sg1 cs))]))
                    (subgoal-facts sg2)))
        (let ([sg2 (make-subgoal s empty (list (cons sg1 c)))])
          (literal-tbl-replace! subgoals s sg2)
          (search! sg2))))
  (define (add-clause! sg c)
    (match c
      [(struct clause (_ lit (list)))
       (fact! sg lit)]
      [(struct clause (_ _ (list-rest selected _)))
       (rule! sg c selected)]))
  (define (search-theory! sg)
    (for-each 
     (lambda (clause)
       (define renamed (rename-clause clause))
       (define selected (clause-head renamed))
       (cond
         [(unify (subgoal-literal sg) selected)
          => (lambda (env)
               (add-clause! sg (subst-clause env renamed)))]))
     (get thy (subgoal-literal sg))))
  (define (search! sg)
    (match (subgoal-literal sg)
      [(struct literal (srcloc '= (list a b)))
       (define (equal-test a b)
         (when (term-equal? a b)
           (fact! sg (make-literal srcloc '= (list a b)))))
       (cond
         [(unify-term (empty-env) a b)
          => (lambda (env) (equal-test (subst-term env a) (subst-term env b)))]
         [else (equal-test a b)])]
      [_
       (search-theory! sg)]))
  (define sg (make-subgoal lit empty empty))
  (literal-tbl-replace! subgoals lit sg)
  (search! sg)
  (subgoal-facts sg))

(provide/contract
 [safe-clause? (clause? . -> . boolean?)]
 [theory/c contract?]
 [immutable-theory/c contract?]
 [mutable-theory/c contract?]
 [make-mutable-theory (-> mutable-theory/c)]
 [make-immutable-theory (-> immutable-theory/c)]
 [assume (immutable-theory/c safe-clause? . -> . immutable-theory/c)]
 [retract (immutable-theory/c clause? . -> . immutable-theory/c)]
 [assume! (mutable-theory/c safe-clause? . -> . void)]
 [retract! (mutable-theory/c clause? . -> . void)]
 [prove (theory/c literal? . -> . (listof literal?))])