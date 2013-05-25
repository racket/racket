#lang racket/base
(require racket/list
         racket/match
         racket/contract
         racket/dict
         "../ast.rkt"
         "env.rkt")

; Variants
(define (variant-terms env1 env2 ts1 ts2)
  (if (empty? ts1)
      (empty? ts2)
      (and (not (empty? ts2))
           (variant-term 
            env1 env2 
            (first ts1) (first ts2)
            (rest ts1) (rest ts2)))))

(define (variant-term env1 env2 t1 t2 ts1 ts2)
  (or (and (variable? t1) (variable? t2)
           (variant-var 
            env1 env2
            (variable-sym t1) (variable-sym t2)
            ts1 ts2))
      (and (term-equal? t1 t2)
           (variant-terms env1 env2 ts1 ts2))))

(define (variant-var env1 env2 v1 v2 ts1 ts2)
  (match (cons (lookup env1 v1) (lookup env2 v2))
    [(list-rest #f #f)
     (variant-terms 
      (extend env1 v1 (make-variable #f v2))
      (extend env2 v2 (make-variable #f v1))
      ts1 ts2)]
    [(list (struct variable (_ v1-p)) (struct variable (_ v2-p)))
     (and (datum-equal? v1-p v2)
          (datum-equal? v2-p v1)
          (variant-terms env1 env2 ts1 ts2))]
    [_ #f]))

(define (variant? l1 l2)
  (or 
   (and (literal? l1) (literal? l2)
        (datum-equal? (literal-predicate l1)
                      (literal-predicate l2))
        (variant-terms 
         (empty-env) (empty-env)
         (literal-terms l1)
         (literal-terms l2)))
   (and (external? l1) (external? l2)
        (equal? (external-predicate l1)
                (external-predicate l2))
        (variant-terms 
         (empty-env) (empty-env)
         (external-arg-terms l1)
         (external-arg-terms l2))
        (variant-terms 
         (empty-env) (empty-env)
         (external-ans-terms l1)
         (external-ans-terms l2)))))

(define (mem-literal lit ls)
  (ormap (lambda (l) (variant? lit l)) ls))

; Literal Tables modulo variant?
(define (term-hash t recur-hash)
  (cond
    [(variable? t)
     101]
    [(constant? t)
     (recur-hash (constant-value t))]))
(define ((mk-literal-hash recur-hash) q)
  (define-values
    (code terms)
    (match q
      [(? literal? l)
       (values (recur-hash (literal-predicate l))
               (literal-terms l))]
      [(? external? e)
       (values (recur-hash (external-predicate e))
               (append (external-arg-terms e)
                       (external-ans-terms e)))]))
  (let loop ([code code]
             [i 0]
             [terms terms])
    (if (empty? terms)
        code
        (loop (+ code (term-hash (first terms) recur-hash) (* i -7))
              (add1 i)
              (rest terms)))))

(define literal-tbl/c
  (coerce-contract 'variant dict?))
(define (make-literal-tbl)
  (make-custom-hash 
   variant? 
   (mk-literal-hash equal-hash-code)
   (mk-literal-hash equal-secondary-hash-code)))
(define (literal-tbl-find ltbl s)
  (dict-ref ltbl s #f))
(define (literal-tbl-replace! ltbl s x)
  (dict-set! ltbl s x))

(provide/contract
 [literal-tbl/c contract?]
 [make-literal-tbl (-> literal-tbl/c)]
 [literal-tbl-find (literal-tbl/c question/c . -> . (or/c false/c any/c))]
 [literal-tbl-replace! (literal-tbl/c question/c any/c . -> . void)]
 [mem-literal (question/c (listof question/c) . -> . boolean?)])
