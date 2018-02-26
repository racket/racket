#lang racket/base
(require "match.rkt"
         "vehicle.rkt"
         "id.rkt")

;; See also "vehicle.rkt" for the `lam` structure type.

(provide extract-lambdas!)

(define (extract-lambdas! lambdas e)
  (match e
    [`(define ,id ,rhs)
     (extract-lambdas! lambdas rhs)]
    [`(define-values ,_ ,rhs)
     (extract-lambdas! lambdas rhs)]
    [`(begin ,es ...)
     (for ([e (in-list es)])
       (extract-lambdas! lambdas e))]
    [`(begin0 ,es ...)
     (extract-lambdas! lambdas `(begin . ,es))]
    [`(lambda ,ids . ,body)
     (hash-set! lambdas e (make-lam (genid 'c_lambda) e))
     (extract-lambdas! lambdas `(begin . ,body))]
    [`(case-lambda [,idss . ,bodys] ...)
     (hash-set! lambdas e (make-lam (genid 'c_case_lambda) e))
     (for ([ids (in-list idss)]
           [body (in-list bodys)])
       (extract-lambdas! lambdas `(begin . ,body)))]
    [`(quote ,_) lambdas]
    [`(if ,tst ,thn ,els)
     (extract-lambdas! lambdas tst)
     (extract-lambdas! lambdas thn)
     (extract-lambdas! lambdas els)]
    [`(with-continuation-mark ,key ,val ,body)
     (extract-lambdas! lambdas key)
     (extract-lambdas! lambdas val)
     (extract-lambdas! lambdas body)]
    [`(let . ,_)
     (extract-let-lambdas! lambdas e)]
    [`(letrec . ,_)
     (extract-let-lambdas! lambdas e)]
    [`(letrec* . ,_)
     (extract-let-lambdas! lambdas e)]
    [`(set! ,id ,rhs)
     (extract-lambdas! lambdas rhs)]
    [`(call-with-values (lambda () . ,body1) (lambda (,ids ...) . ,body2))
     (extract-lambdas! lambdas `(begin . ,body1))
     (extract-lambdas! lambdas `(begin . ,body2))]
    [`(,rator ,rands ...)
     (extract-lambdas! lambdas `(begin ,rator . ,rands))]
    [`,_ (void)]))

(define (extract-let-lambdas! lambdas e)
  (match e
    [`(,let-id ([,ids ,rhss] ...) . ,body)
     (for ([rhs (in-list rhss)])
       (extract-lambdas! lambdas rhs))
     (extract-lambdas! lambdas `(begin . ,body))]))
