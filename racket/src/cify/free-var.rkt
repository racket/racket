#lang racket/base
(require "match.rkt"
         "vehicle.rkt"
         "function.rkt"
         "ref.rkt"
         "sort.rkt"
         "arg.rkt")

(provide get-free-vars)

(define (get-free-vars e env lambdas knowns top-names state)
  (define lam (hash-ref lambdas e))
  (or (lam-free-var-refs lam)
      (let ([vars (extract-lambda-free-vars #hasheq() e env lambdas knowns top-names state)])
        (define free-vars (for/list ([var (in-sorted-hash-keys vars symbol<?)]
                                     #:when (hash-ref env var #f))
                            var))
        (define free-var-refs (map make-ref free-vars))
        (set-lam-free-var-refs! lam free-var-refs)
        (set-lam-env! lam env)
        free-var-refs)))

(define (extract-lambda-free-vars vars e env lambdas knowns top-names state)
  (match e
    [`(lambda ,ids . ,body)
     (extract-free-vars vars `(begin . ,body) (add-args env ids) lambdas knowns top-names state)]
    [`(case-lambda [,idss . ,bodys] ...)
     (for/fold ([vars vars]) ([ids (in-list idss)]
                              [body (in-list bodys)])
       (extract-free-vars vars `(begin . ,body) (add-args env ids) lambdas knowns top-names state))]))

(define (extract-free-vars vars e env lambdas knowns top-names state)
  (match e
    [`(begin ,es ...)
     (for/fold ([vars vars]) ([e (in-list es)])
       (extract-free-vars vars e env lambdas knowns top-names state))]
    [`(begin0 ,es ...)
     (extract-free-vars vars `(begin . ,es) env lambdas knowns top-names state)]
    [`(lambda . ,_)
     (add-args vars (map ref-id (get-free-vars e env lambdas knowns top-names state)))]
    [`(case-lambda . ,_)
     (add-args vars (map ref-id (get-free-vars e env lambdas knowns top-names state)))]
    [`(quote ,_) vars]
    [`(if ,tst ,thn ,els)
     (define vars1 (extract-free-vars vars tst env lambdas knowns top-names state))
     (define vars2 (extract-free-vars vars1 thn env lambdas knowns top-names state))
     (extract-free-vars vars2 els env lambdas knowns top-names state)]
    [`(with-continuation-mark ,key ,val ,body)
     (define vars1 (extract-free-vars vars key env lambdas knowns top-names state))
     (define vars2 (extract-free-vars vars1 val env lambdas knowns top-names state))
     (extract-free-vars vars2 body env lambdas knowns top-names state)]
    [`(let . ,_)
     (extract-let-free-vars vars e env lambdas knowns top-names state)]
    [`(letrec . ,_)
     (extract-let-free-vars vars e env lambdas knowns top-names state)]
    [`(letrec* . ,_)
     (extract-let-free-vars vars e env lambdas knowns top-names state)]
    [`(set! ,id ,rhs)
     (define vars1 (if (hash-ref env (unref id) #f)
                       (hash-set vars (unref id) #t)
                       vars))
     (extract-free-vars vars1 rhs env lambdas knowns top-names state)]
    [`(call-with-values (lambda () . ,body1) (lambda (,ids ...) . ,body2))
     (define vars1 (extract-free-vars vars `(begin . ,body1) env lambdas knowns top-names state))
     (extract-free-vars vars1 `(begin . ,body2) (add-args env ids) lambdas knowns top-names state)]
    [`(,rator ,rands ...)
     (cond
       [(function? (hash-ref knowns rator #f))
        (extract-free-vars vars `(begin . ,rands) env lambdas knowns top-names state)]
       [else
        (extract-free-vars vars `(begin ,rator . ,rands) env lambdas knowns top-names state)])]
    [`,_
     (cond
       [(symbol-ref? e)
        (if (hash-ref env (unref e) #f)
            (hash-set vars (unref e) #t)
            vars)]
       [else vars])]))

(define (extract-let-free-vars vars e env lambdas knowns top-names state)
  (match e
    [`(,let-id ([,ids ,rhss] ...) . ,body)
     (define body-env (for/fold ([env env]) ([id (in-list ids)]
                                             #:unless (hash-ref top-names id #f))
                        (hash-set env id #t)))
     (define rhs-env (if (eq? let-id 'let) env body-env))
     (define new-vars
       (for/fold ([vars vars]) ([rhs (in-list rhss)])
         (extract-free-vars vars rhs rhs-env lambdas knowns top-names state)))
     (extract-free-vars new-vars `(begin . ,body) body-env lambdas knowns top-names state)]))
