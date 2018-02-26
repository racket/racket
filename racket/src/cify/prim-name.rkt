#lang racket/base
(require "match.rkt"
         "arg.rkt")

;; Extract all primitives that are referenced

(provide extract-prim-names)

(define (extract-prim-names e top-names)
  (define (extract-prim-names names e env)
    (match e
      [`(define ,_ ,rhs)
       (extract-prim-names names rhs env)]
      [`(define-values ,_ ,rhs)
       (extract-prim-names names rhs env)]
      [`(begin ,es ...)
       (for/fold ([names names]) ([e (in-list es)])
         (extract-prim-names names e env))]
      [`(begin0 ,es ...)
       (extract-prim-names names `(begin . ,es) env)]
      [`(lambda ,ids . ,body)
       (extract-prim-names names `(begin . ,body) (add-args env ids))]
      [`(case-lambda [,idss . ,bodys] ...)
       (for/fold ([names names]) ([ids (in-list idss)]
                                  [body (in-list bodys)])
         (extract-prim-names names `(begin . ,body) (add-args env ids)))]
      [`(quote ,_) names]
      [`(if ,tst ,thn ,els)
       (define names1 (extract-prim-names names tst env))
       (define names2 (extract-prim-names names1 thn env))
       (extract-prim-names names2 els env)]
      [`(with-continuation-mark ,key ,val ,body)
       (define names1 (extract-prim-names names key env))
       (define names2 (extract-prim-names names1 val env))
       (extract-prim-names names2 body env)]
      [`(let . ,_)
       (extract-let-prim-names names e env)]
      [`(letrec . ,_)
       (extract-let-prim-names names e env)]
      [`(letrec* . ,_)
       (extract-let-prim-names names e env)]
      [`(set! ,id ,rhs)
       (extract-prim-names names rhs env)]
      [`(#%app . ,r)
       (extract-prim-names names r env)]
      [`(,rator ,rands ...)
       (extract-prim-names names `(begin ,rator . ,rands) env)]
      [`,_
       (cond
         [(symbol? e)
          (cond
            [(or (hash-ref env e #f)
                 (hash-ref top-names e #f))
             names]
            [else
             (hash-set names e #t)])]
         [else names])]))

  (define (extract-let-prim-names names e env)
    (match e
      [`(,let-id ([,ids ,rhss] ...) . ,body)
       (define body-env (for/fold ([env env]) ([id (in-list ids)])
                          (if (hash-ref top-names id #f)
                              env
                              (hash-set env id #t))))
       (define rhs-env (if (eq? let-id 'let) env body-env))
       (define new-names
         (for/fold ([names names]) ([rhs (in-list rhss)])
           (extract-prim-names names rhs rhs-env)))
       (extract-prim-names new-names `(begin . ,body) body-env)]))

  (extract-prim-names #hasheq() e #hasheq()))
