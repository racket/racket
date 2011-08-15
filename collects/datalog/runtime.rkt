#lang racket/base
(require racket/contract
         racket/list
         racket/match
         racket/function
         "ast.rkt"
         "private/env.rkt"
         "private/subst.rkt"
         "private/unify.rkt"
         "private/variant.rkt")

; A clause is safe if every variable in its head occurs in some literal in its body.
(define (safe-clause? c)
  (define head-vars (filter variable? (literal-terms (clause-head c))))
  (andmap (lambda (v)
            (ormap (lambda (l)
                     (ormap (lambda (t) (term-equal? t v))
                            (cond
                              [(literal? l)
                               (literal-terms l)]
                              [(external? l)
                               (append (external-arg-terms l)
                                       (external-ans-terms l))])))
                   (clause-body c)))
          head-vars))

(define theory/c (and/c hash? (not/c immutable?)))
(define (literal-key l)
  (format "~a/~a" (literal-predicate l) (length (literal-terms l))))
(define (clause-key c)
  (literal-key (clause-head c)))

(define (make-theory)
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
  (question 
   [facts #:mutable]
   [waiters #:mutable]))

(define (resolve c q)
  (define body (clause-body c))
  (and (not (empty? body))
       (cond
         [(unify (first body) (rename-question q))
          => (lambda (env)
               (subst-clause env (make-clause (clause-srcloc c) (clause-head c) (rest body))))]
         [else #f])))

(define (prove thy q)
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
         [(unify (subgoal-question sg) selected)
          => (lambda (env)
               (add-clause! sg (subst-clause env renamed)))]))
     (get thy (subgoal-question sg))))
  (define (search! sg)
    (match (subgoal-question sg)
      [(external srcloc pred-sym pred args anss)
       (and (andmap constant? args)
            (call-with-values 
             (λ ()
               (apply pred (map constant-value args)))
             (λ resolved-vals
               (define resolved-anss
                 (map (curry constant #f)
                      resolved-vals))
               (cond
                 [(unify-terms (empty-env) anss resolved-anss)
                  => (λ (env)
                       (fact! sg (external srcloc pred-sym pred args (subst-terms env anss))))]))))]
      [(struct literal (srcloc '= (list a b)))
       (define (equal-test a b)
         (when (term-equal? a b)
           (fact! sg (make-literal srcloc '= (list a b)))))
       (cond
         [(unify-term (empty-env) a b)
          => (lambda (env) (equal-test (subst-term env a) (subst-term env b)))]
         [else (equal-test a b)])]
      [(struct literal (srcloc '!= (list a b)))
       (define (equal-test a b)
         (unless (term-equal? a b)
           (fact! sg (make-literal srcloc '!= (list a b)))))
       (cond
         [(unify-term (empty-env) a b)
          => (lambda (env) (equal-test (subst-term env a) (subst-term env b)))]
         [else (equal-test a b)])]
      [_
       (search-theory! sg)]))
  (define sg (make-subgoal q empty empty))
  (literal-tbl-replace! subgoals q sg)
  (search! sg)
  (subgoal-facts sg))

(provide/contract
 [safe-clause? (clause? . -> . boolean?)]
 [theory/c contract?]
 [make-theory (-> theory/c)]
 [assume! (theory/c safe-clause? . -> . void)]
 [retract! (theory/c clause? . -> . void)]
 [prove (theory/c question/c . -> . (listof question/c))])
