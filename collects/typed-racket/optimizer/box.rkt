#lang racket/base

(require syntax/parse unstable/syntax
         racket/match
         "../utils/utils.rkt"
         (for-template racket/base racket/unsafe/ops)
         (utils tc-utils)
         (rep type-rep)
         (types type-table utils)
         (optimizer utils logging))

(provide box-opt-expr)

(define-syntax-class box-expr
  #:commit
  (pattern e:expr
           #:when (match (type-of #'e)
                    [(tc-result1: (Box: _)) #t]
                    [_ #f])
           #:with opt ((optimize) #'e)))

(define-syntax-class box-op
  #:commit
  ;; we need the * versions of these unsafe operations to be chaperone-safe
  (pattern (~literal unbox)    #:with unsafe #'unsafe-unbox)
  (pattern (~literal set-box!) #:with unsafe #'unsafe-set-box!))

(define-syntax-class box-opt-expr
  #:commit
  (pattern (#%plain-app op:box-op b:box-expr new:expr ...)
           #:with opt
           (begin (log-optimization "box" "Box check elimination." this-syntax)
                  (add-disappeared-use #'op)
                  #`(op.unsafe b.opt #,@(syntax-map (optimize) #'(new ...))))))
