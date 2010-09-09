#lang scheme/base

(require syntax/parse
         racket/match
         (for-template scheme/base scheme/unsafe/ops)
         "../utils/utils.rkt"
         (rep type-rep)
         (types type-table utils)
         (typecheck typechecker)
         (optimizer utils))

(provide pair-opt-expr)


(define-syntax-class pair-op
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
  (pattern e:pair-derived-opt-expr
           #:with opt
           (begin (log-optimization "derived pair" #'e)
                  #'e.opt))
  (pattern (#%plain-app op:pair-op p:pair-expr)
           #:with opt
           (begin (log-optimization "pair" #'op)
                  #'(op.unsafe p.opt)))
  (pattern (#%plain-app op:mpair-op p:mpair-expr e:expr ...)
           #:with opt
           (begin (log-optimization "mutable pair" #'op)
                  #`(op.unsafe p.opt #,@(map (optimize) (syntax->list #'(e ...)))))))

  
;; if the equivalent sequence of cars and cdrs is guaranteed not to fail,
;; we can optimize

;; accessors is a list of syntax objects, all #'car or #'cdr
(define (gen-alt accessors stx)
  (syntax-parse stx
    [(#%plain-app op arg)
     (define (gen-alt-helper accessors)
       (if (null? accessors)
           #'arg
           #`(#%plain-app #,(car accessors)
                          #,(gen-alt-helper (cdr accessors)))))
     (let ((ty  (type-of stx))
           (obj (gen-alt-helper accessors)))
       ;; we're calling the typechecker, but this is just a shortcut, we're
       ;; still conceptually single pass (we're not iterating). we could get
       ;; the same result by statically destructing the types.
       (tc-expr/check obj ty)
       obj)]))

(define-syntax-class pair-derived-expr
  #:commit
  (pattern (#%plain-app (~literal caar) x)
           #:with alt (gen-alt (list #'car #'car) this-syntax))
  (pattern (#%plain-app (~literal cadr) x)
           #:with alt (gen-alt (list #'car #'cdr) this-syntax))
  (pattern (#%plain-app (~literal cdar) x)
           #:with alt (gen-alt (list #'cdr #'car) this-syntax))
  (pattern (#%plain-app (~literal cddr) x)
           #:with alt (gen-alt (list #'cdr #'cdr) this-syntax)))

(define-syntax-class pair-derived-opt-expr
  #:commit
  (pattern e:pair-derived-expr
           #:with e*:pair-opt-expr #'e.alt
           #:with opt #'e*.opt))
