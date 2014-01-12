#lang racket/base

(require "../utils/utils.rkt" syntax/parse
         (contract-req)
         (rep type-rep)
         (env lexical-env)
         (private type-annotation)
         (for-template racket/base))

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
  #:literals (reverse letrec-syntaxes+values let-values #%plain-app
                      if letrec-values begin #%plain-lambda set! case-lambda
                      begin0 with-continuation-mark)
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

;; expr id -> type or #f
;; if there is a binding in stx of the form:
;; (let ([x (reverse name)]) e) or
;; (let ([x name]) e)
;; where x has a type annotation, return that annotation, otherwise #f
(define (find-annotation stx name)
  (define (find s) (find-annotation s name))
  (define-syntax-class annotated
    #:attributes (type)
    (pattern i:id
      #:attr type (type-annotation #'i)
      #:when (attribute type))
    (pattern :typed-id/lexical^)
    (pattern _
      #:attr type #f))
  (define-syntax-class annotated-lv-clause
    #:attributes (type)
    #:literals (#%plain-app reverse)
    (pattern [(:annotated) (~or (#%plain-app reverse n:id) n:id)]
     #:when (free-identifier=? name #'n))
    (pattern _
     #:attr type #f))
  (syntax-parse stx
    #:literals (let-values)
    [(let-values (cls:annotated-lv-clause ...) body)
     (or (ormap values (attribute cls.type))
         (find #'body))]
    [e:core-expr
     (ormap find (syntax->list #'(e.expr ...)))]))
