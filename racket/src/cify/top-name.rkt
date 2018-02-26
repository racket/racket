#lang racket/base
(require "match.rkt")

;; Get the names are are defined at the top level of `let[rec[*]]`
;; bindings that should be flattened into the top level. In other
;; words, stop collecting names at a `lambda`.

(provide extract-top-names)

(define (extract-top-names names e)
  (match e
    [`(define ,id ,rhs)
     (extract-top-names (hash-set names id #t) rhs)]
    [`(define-values (,ids ...) ,rhs)
     (define new-names
       (for/fold ([names names]) ([id (in-list ids)])
         (hash-set names id #t)))
     (extract-top-names new-names rhs)]
    [`(begin ,es ...)
     (for/fold ([names names]) ([e (in-list es)])
       (extract-top-names names e))]
    [`(begin0 ,es ...)
     (extract-top-names names `(begin . ,es))]
    [`(lambda ,ids . ,body)
     names]
    [`(case-lambda [,idss . ,bodys] ...)
     names]
    [`(quote ,_) names]
    [`(if ,tst ,thn ,els)
     (define names1 (extract-top-names names tst))
     (define names2 (extract-top-names names1 thn))
     (extract-top-names names2 els)]
    [`(with-continuation-mark ,key ,val ,body)
     (define names1 (extract-top-names names key))
     (define names2 (extract-top-names names1 val))
     (extract-top-names names2 body)]
    [`(let . ,_)
     (extract-let-top-names names e)]
    [`(letrec . ,_)
     (extract-let-top-names names e)]
    [`(letrec* . ,_)
     (extract-let-top-names names e)]
    [`(set! ,id ,rhs)
     (extract-top-names names rhs)]
    [`(#%app . ,r)
     (extract-top-names names r)]
    [`(,rator ,rands ...)
     (extract-top-names names `(begin ,rator . ,rands))]
    [`,_ names]))


(define (extract-let-top-names names e)
  (match e
    [`(,let-id ([,ids ,rhss] ...) . ,body)
     (define new-names
       (for/fold ([names names]) ([id (in-list ids)]
                                  [rhs (in-list rhss)])
         (extract-top-names (hash-set names id #t) rhs)))
     (extract-top-names new-names `(begin . ,body))]))
  
