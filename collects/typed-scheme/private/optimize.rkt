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
  #:local-conventions ([#px"^e" opt-expr]
                       [#px"^f\\d*s?$" float-opt-expr]
                       [#px"^p\\d*s?$" pair-opt-expr])
  (pattern (#%plain-app op:float-unary-op f)
           #:with opt #'(op.unsafe f.opt))
  ;; unlike their safe counterparts, unsafe binary operators can only take 2 arguments
  (pattern (#%plain-app op:float-binary-op f fs ...)
           #:with opt 
           (for/fold ([o #'f.opt])
             ([e (syntax->list #'(fs.opt ...))])
             #`(op.unsafe #,o #,e)))
  (pattern (#%plain-app op:pair-unary-op p)
           #:with opt #'(op.unsafe p.opt))

  ;; boring cases, just recur down
  (pattern (#%plain-lambda formals e ...)
           #:with opt #'(#%plain-lambda formals e.opt ...))
  (pattern (define-values formals e ...)
           #:with opt #'(define-values formals e.opt ...))
  (pattern (case-lambda [formals e ...] ...)
           #:with opt #'(case-lambda [formals e.opt ...] ...))
  (pattern (let-values ([ids e-rhs] ...) e-body ...)
           #:with opt #'(let-values ([ids e-rhs.opt] ...) e-body.opt ...))
  (pattern (letrec-values ([ids e-rhs] ...) e-body ...)
           #:with opt #'(letrec-values ([ids e-rhs.opt] ...) e-body.opt ...))
  (pattern (letrec-syntaxes+values stx-bindings ([(ids ...) e-rhs] ...) e-body ...)
           #:with opt #'(letrec-syntaxes+values stx-bindings ([(ids ...) e-rhs.opt] ...) e-body.opt ...))
  (pattern (kw:identifier expr ...)
           #:when (ormap (lambda (k) (free-identifier=? k #'kw))
                         (list #'if #'begin #'begin0 #'set! #'#%plain-app #'#%app #'#%expression
                               #'#%variable-reference #'with-continuation-mark))
           #:with opt #'(kw expr.opt ...))
  (pattern other:expr
           #:with opt #'other))

(define (optimize stx)
  (syntax-parse stx #:literal-sets (kernel-literals)
    [e:opt-expr
     (syntax/loc stx e.opt)]))
