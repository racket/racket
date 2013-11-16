#lang racket/base

(require "../utils/utils.rkt" syntax/parse
         (contract-req)
         (rep type-rep)
         (env lexical-env)
         (private type-annotation)
         (for-label racket/base))

(provide/cond-contract [find-annotation (syntax? identifier? . -> . (or/c #f Type/c))])

(define-syntax-class lv-clause
  #:transparent
  (pattern [(v:id ...) e:expr]))

(define-syntax-class lv-clauses
  #:transparent
  (pattern (cl:lv-clause ...)
           #:with (e ...) #'(cl.e ...)
           #:with (vs ...) #'((cl.v ...) ...)))

(define-syntax-class core-expr
  #:literal-sets (kernel-literals)
  #:transparent
  (pattern (let-values cls:lv-clauses body)
           #:with (expr ...) #'(cls.e ... body))
  (pattern (letrec-values cls:lv-clauses body)
           #:with (expr ...) #'(cls.e ... body))
  (pattern (letrec-syntaxes+values _ cls:lv-clauses body)
           #:with (expr ...) #'(cls.e ... body))
  (pattern (#%plain-app expr ...))
  (pattern (if expr ...))
  (pattern (with-continuation-mark expr ...))
  (pattern (begin expr ...))
  (pattern (begin0 expr ...))
  (pattern (#%plain-lambda _ e)
           #:with (expr ...) #'(e))
  (pattern (case-lambda [_ expr] ...))
  (pattern (set! _ e)
           #:with (expr ...) #'(e))
  (pattern _
           #:with (expr ...) #'()))

(define-literal-set find-annotation-literals #:for-label
    (reverse))

;; expr id -> type or #f
;; if there is a binding in stx of the form:
;; (let ([x (reverse name)]) e) or
;; (let ([x name]) e)
;; where x has a type annotation, return that annotation, otherwise #f
(define (find-annotation stx name)
  (define (find s) (find-annotation s name))
  (define (match? b)
    (syntax-parse b
      #:literal-sets (kernel-literals find-annotation-literals)
      [c:lv-clause
       #:with n:id #'c.e
       #:with (v) #'(c.v ...)
       #:fail-unless (free-identifier=? name #'n) #f
       (or (type-annotation #'v) (lookup-type/lexical #'v #:fail (lambda _ #f)))]
      [c:lv-clause
       #:with (#%plain-app reverse n:id) #'c.e
       #:with (v) #'(c.v ...)
       #:fail-unless (free-identifier=? name #'n) #f
       (or (type-annotation #'v) (lookup-type/lexical #'v #:fail (lambda _ #f)))]
      [_ #f]))
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(let-values cls:lv-clauses body)
     (or (ormap match? (syntax->list #'cls))
	 (find #'body))]
    [e:core-expr
     (ormap find (syntax->list #'(e.expr ...)))]))
