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
  (pattern (~literal car) #:with unsafe #'unsafe-car)
  (pattern (~literal cdr) #:with unsafe #'unsafe-cdr))

(define-syntax-class pair-expr
  (pattern e:expr
           #:when (match (type-of #'e) ; type of the operand
                    [(tc-result1: (Pair: _ _)) #t]
                    [_ #f])
           #:with opt ((optimize) #'e)))

(define-syntax-class pair-opt-expr
  (pattern (#%plain-app op:pair-unary-op p:pair-expr)
           #:with opt
           (begin (log-optimization "unary pair" #'op)
                  #'(op.unsafe p.opt))))
