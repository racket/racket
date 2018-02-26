#lang racket/base
(require racket/list
         racket/match
         "../host/correlate.rkt"
         "../run/status.rkt"
         (prefix-in bootstrap: "../run/linklet.rkt")
         "defn.rkt")

(provide prune-names)

;; Remove function names reported with `quote` when the
;; name is redundant after all transformations.
(define (prune-names linklet-expr)
  (define body (bootstrap:s-expr-linklet-body linklet-expr))

  (define new-body
    (for/list ([e (in-list body)])
      (cond
        [(defn? e)
         (define ids (defn-syms e))
         `(define-values ,ids ,(prune (defn-rhs e) (get-single-id ids)))]
        [else
         (prune e #f)])))
  
  (append (take linklet-expr 3)
          new-body))

(define (prune e id)
  (match e
    [`(lambda ,args (begin (quote ,name-id) ,es ...))
     `(lambda ,args ,(if (eq? name-id id)
                         (prune `(begin . ,es) #f)
                         (prune `(begin (quote ,name-id) ,@es) #f)))]
    [`(lambda ,args ,e)
     `(lambda ,args ,(prune e #f))]
    [`(case-lambda [,args (begin (quote ,name-id) ,es ...)]
                   [,argss ,bodys] ...)
     `(case-lambda
        [,args ,(if (eq? name-id id)
                    (prune `(begin . ,es) #f)
                    (prune `(begin (quote ,name-id) ,@es) #f))]
        ,@(for/list ([args (in-list argss)]
                     [body (in-list bodys)])
            `[,args ,(prune body #f)]))]
    [`(case-lambda [,argss ,bodys] ...)
     `(case-lambda
        ,@(for/list ([args (in-list argss)]
                     [body (in-list bodys)])
            `[,args ,(prune body #f)]))]
    [`(let-values ([,idss ,rhss] ...) ,e)
     `(let-values ,(for/list ([ids (in-list idss)]
                              [rhs (in-list rhss)])
                     `[,ids ,(prune rhs (get-single-id ids))])
        ,(prune e id))]
    [`(letrec-values ([,idss ,rhss] ...) ,e)
     `(letrec-values ,(for/list ([ids (in-list idss)]
                                 [rhs (in-list rhss)])
                        `[,ids ,(prune rhs (get-single-id ids))])
        ,(prune e id))]
    [`(if ,tst ,thn ,els)
     `(if ,(prune tst #f) ,(prune thn id) ,(prune els id))]
    [`(with-continuation-mark ,key ,val ,body)
     `(with-continuation-mark ,(prune key #f) ,(prune val #f) ,(prune body id))]
    [`(quote ,_) e]
    [`(#%variable-reference . ,_) e]
    [`(set! ,id ,e)
     `(set! ,id ,(prune e id))]
    [`(,rator ,rands ...)
     (cons (prune rator #f)
           (for/list ([rand (in-list rands)])
             (prune rand #f)))]
    [else e]))

(define (get-single-id ids)
  (and (pair? ids)
       (null? (cdr ids))
       (car ids)))
