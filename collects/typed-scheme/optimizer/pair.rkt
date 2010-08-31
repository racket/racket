#lang scheme/base

(require syntax/parse
         syntax/id-table racket/dict
         unstable/match scheme/match
         (for-template scheme/base scheme/unsafe/ops)
         "../utils/utils.rkt"
         (rep type-rep)
         (types abbrev type-table utils subtype)
         (optimizer utils))

(provide pair-opt-expr)


(define-syntax-class pair-unary-op
  #:commit
  (pattern (~literal car) #:with unsafe #'unsafe-car)
  (pattern (~literal cdr) #:with unsafe #'unsafe-cdr))
(define-syntax-class mpair-op
  #:commit
  (pattern (~literal mcar) #:with unsafe #'unsafe-mcar)
  (pattern (~literal mcdr) #:with unsafe #'unsafe-mcdr)
  (pattern (~literal set-mcar!) #:with unsafe #'unsafe-set-mcar!)
  (pattern (~literal set-mcdr!) #:with unsafe #'unsafe-set-mcdr!))


(define-syntax-class pair-expr
  #:commit
  (pattern e:expr
           #:when (match (type-of #'e) ; type of the operand
                    [(tc-result1: (Pair: _ _)) #t]
                    [_ #f])
           #:with opt ((optimize) #'e)))
(define-syntax-class mpair-expr
  #:commit
  (pattern e:expr
           #:when (match (type-of #'e) ; type of the operand
                    [(tc-result1: (MPair: _ _)) #t]
                    [_ #f])
           #:with opt ((optimize) #'e)))

(define-syntax-class pair-opt-expr
  #:commit
  (pattern (#%plain-app op:pair-unary-op p:pair-expr)
           #:with opt
           (begin (log-optimization "unary pair" #'op)
                  #'(op.unsafe p.opt)))
  (pattern (#%plain-app op:mpair-op p:mpair-expr e:expr ...)
           #:with opt
           (begin (log-optimization "mutable pair" #'op)
                  #`(op.unsafe p.opt #,@(map (optimize) (syntax->list #'(e ...)))))))
