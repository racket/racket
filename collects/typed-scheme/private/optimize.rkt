#lang scheme/base

(require syntax/parse (for-template scheme/base scheme/unsafe/ops)
         "../utils/utils.rkt" unstable/match scheme/match unstable/syntax
         (rep type-rep)
         (types abbrev type-table utils))
(provide optimize)

(define-syntax-class float-opt-expr
  (pattern e:opt-expr
           #:when (match (type-of #'e)
                    [(tc-result1: (== -Flonum type-equal?)) #t] [_ #f])
           #:with opt #'e.opt))

(define-syntax-class float-binary-op
  #:literals (+ - * / = <= < > >= min max)
  (pattern (~and i:id (~or + - * / = <= < > >= min max))
           #:with unsafe (format-id #'here "unsafe-fl~a" #'i)))

(define-syntax-class float-unary-op
  #:literals (abs sin cos tan asin acos atan log exp)
  (pattern (~and i:id (~or abs sin cos tan asin acos atan log exp))
           #:with unsafe (format-id #'here "unsafe-fl~a" #'i)))

(define-syntax-class pair-opt-expr
  (pattern e:opt-expr
           #:when (match (type-of #'e) ; type of the operand
                    [(tc-result1: (Pair: _ _)) #t]
                    [_ #f])
           #:with opt #'e.opt))

(define-syntax-class pair-unary-op
  #:literals (car cdr)
  (pattern (~and i:id (~or car cdr))
           #:with unsafe (format-id #'here "unsafe-~a" #'i)))

(define-syntax-class opt-expr
  (pattern e:opt-expr*
           #:with opt (syntax-recertify #'e.opt this-syntax (current-code-inspector) #f)))

(define-syntax-class opt-expr*
  #:literal-sets (kernel-literals)
  #:local-conventions ([#rx"^e" opt-expr]
                       [#rx"^f" float-opt-expr]
                       [#rx"^p" pair-opt-expr])
  (pattern (let-values ([ids e-rhs] ...) e-body ...)
           #:with opt #'(let-values ([ids e-rhs.opt] ...) e-body.opt ...))
  (pattern (#%plain-app op:float-unary-op f)
           #:with opt #'(op.unsafe f.opt))
  (pattern (#%plain-app op:float-binary-op f fs ...)
           #:with opt 
           (for/fold ([o #'f.opt])
             ([e (syntax->list #'(fs.opt ...))])
             #`(op.unsafe #,o #,e)))
  (pattern (#%plain-app op:pair-unary-op p)
           #:with opt #'(op.unsafe p.opt))
  (pattern (#%plain-app e ...)
           #:with opt #'(#%plain-app e.opt ...))
  (pattern (lambda (x ...) e ...)
           #:with opt #'(lambda (x ...) e.opt ...))
  (pattern other:expr
           #:with opt #'other))

(define (optimize stx)
  (syntax-parse stx #:literal-sets (kernel-literals)
    [(define-values ~! ids e:opt-expr)
     (syntax/loc stx (define-values ids e.opt))]
    [(#%app e:opt-expr ...) (syntax/loc stx (#%app e.opt ...))]
    [(#%plain-app e:opt-expr ...) (syntax/loc stx (#%plain-app e.opt ...))]
    [_ stx]))
