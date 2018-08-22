#lang racket/base
(require "match.rkt"
         "id.rkt")

;; Some schemify passes reuse identifiers, breaking the original
;; input's property of having a unique symbol for every binding. The
;; `re-unique` function recovers that property.

(provide re-unique)

(define (re-unique e)
  (define all-ids (make-hasheq))

  ;; Give precedence to top-level names so they
  ;; don't change:
  (define (get-top-names e)
    (match e
      [`(define ,id ,rhs)
       (hash-set! all-ids id (no-c-prefix id))]
      [`(define-values ,ids ,rhs)
       (for ([id (in-list ids)])
         (hash-set! all-ids id (no-c-prefix id)))]
      [`(begin . ,es)
       (for ([e (in-list es)])
         (get-top-names e))]
      [`,_ (void)]))

  (define (select-unique ids)
    (cond
      [(null? ids) '()]
      [(symbol? ids)
       (cond
         [(hash-ref all-ids ids #f)
          (define new-id (gensym ids))
          (hash-set! all-ids ids (no-c-prefix new-id))
          new-id]
         [else
          (hash-set! all-ids ids (no-c-prefix ids))
          ids])]
      [else
       (cons (select-unique (car ids))
             (select-unique (cdr ids)))]))

  (define (re-unique e env)
    (match e
      [`(define ,id ,rhs)
       `(define ,(no-c-prefix id) ,(re-unique rhs env))]
      [`(define-values ,ids ,rhs)
       `(define-values ,(map no-c-prefix ids) ,(re-unique rhs env))]
      [`(begin . ,body)
       `(begin . ,(re-unique-body body env))]
      [`(begin0 . ,body)
       `(begin0 . ,(re-unique-body body env))]
      [`(lambda ,ids . ,body)
       (define new-ids (select-unique ids))
       `(lambda ,new-ids . ,(re-unique-body body (env-add env ids new-ids)))]
      [`(case-lambda [,idss . ,bodys] ...)
       `(case-lambda
         ,@(for/list ([ids (in-list idss)]
                      [body (in-list bodys)])
             (define new-ids (select-unique ids))
             `[,new-ids . ,(re-unique-body body (env-add env ids new-ids))]))]
      [`(if ,tst ,thn ,els)
       `(if ,(re-unique tst env) ,(re-unique thn env) ,(re-unique els env))]
      [`(with-continuation-mark ,key ,val ,body)
       `(with-continuation-mark ,(re-unique key env) ,(re-unique val env) ,(re-unique body env))]
      [`(let . ,_) (re-unique-let e env)]
      [`(letrec . ,_) (re-unique-let e env)]
      [`(letrec* . ,_) (re-unique-let e env)]
      ;; Recognize `values` form so we can use this on schemify input, too
      [`(let-values . ,_) (re-unique-let-values e env)]
      [`(letrec-values . ,_) (re-unique-let-values e env)]
      [`(set! ,id ,rhs)
       `(set! ,(re-unique id env) ,(re-unique rhs env))]
      [`(,rator ,rands ...)
       (re-unique-body e env)]
      [`,_
       (cond
         [(symbol? e) (hash-ref env e e)]
         [else e])]))

  (define (re-unique-body body env)
    (for/list ([e (in-list body)])
      (re-unique e env)))
  
  (define (re-unique-let e env)
    (match e
      [`(,let-id ([,ids ,rhss] ...) . ,body)
       (define rec? (not (eq? let-id 'let)))
       (define new-ids (select-unique ids))
       (define body-env (env-add env ids new-ids))
       (define rhs-env (if rec? body-env env))
       `(,let-id ,(for/list ([id (in-list new-ids)]
                             [rhs (in-list rhss)])
                    `[,id ,(re-unique rhs rhs-env)])
                 . ,(re-unique-body body body-env))]))

  (define (re-unique-let-values e env)
    (match e
      [`(,let-values-id ([(,idss ...) ,rhss] ...) . ,body)
       (define rec? (not (eq? let-values-id 'let-values)))
       (define new-idss (map select-unique idss))
       (define body-env (for/fold ([env env]) ([ids (in-list idss)]
                                               [new-ids (in-list new-idss)])
                          (env-add env ids new-ids)))
       (define rhs-env (if rec? body-env env))
       `(,let-values-id ,(for/list ([ids (in-list new-idss)]
                                    [rhs (in-list rhss)])
                           `[,ids ,(re-unique rhs rhs-env)])
                        . ,(re-unique-body body body-env))]))

  (define (env-add env ids new-ids)
    (cond
      [(null? ids) env]
      [(symbol? ids)
       (if (eq? ids new-ids)
           env
           (hash-set env ids new-ids))]
      [else
       (env-add (env-add env (car ids) (car new-ids)) (cdr ids) (cdr new-ids))]))

  (get-top-names e)
  (re-unique e #hasheq()))
