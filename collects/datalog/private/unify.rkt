#lang racket/base
(require racket/match
         racket/list
         racket/contract
         "../ast.rkt"
         "env.rkt")

(define (chase env t)
  (match t
    [(struct variable (_ var))
     (cond
       [(lookup env var)
        => (lambda (term) (chase env term))]
       [else t])]
    [_ t]))

(define (unify-term env t1 t2)
  (define t1-p (chase env t1))
  (define t2-p (chase env t2))
  (if (term-equal? t1-p t2-p)
      env
      (match t1-p
        [(struct variable (_ var))
         (extend env var t2-p)]
        [_
         (match t2-p
           [(struct variable (_ var))
            (extend env var t1-p)]
           [_
            #f])])))

(define (unify-terms env ts1 ts2)
  (if (empty? ts1)
      (if (empty? ts2)
          env
          #f)
      (if (empty? ts2)
          #f
          (match (unify-term env (first ts1) (first ts2))
            [#f #f]
            [env (unify-terms env (rest ts1) (rest ts2))]))))      

(define (unify l1 l2)
  (or (and (literal? l1) (literal? l2)
           (datum-equal? (literal-predicate l1)
                         (literal-predicate l2))
           (unify-terms (empty-env)
                        (literal-terms l1)
                        (literal-terms l2)))
      (and (external? l1) (external? l2)
           (equal? (external-predicate l1)
                   (external-predicate l2))
           (unify-terms (empty-env)
                        (append (external-arg-terms l1)
                                (external-ans-terms l1))
                        (append (external-arg-terms l2)
                                (external-ans-terms l2))))))

(provide/contract
 [unify (question/c question/c . -> . (or/c false/c env/c))]
 [unify-terms (env/c (listof term/c) (listof term/c) . -> . (or/c false/c env/c))]
 [unify-term (env/c term/c term/c . -> . (or/c false/c env/c))])
