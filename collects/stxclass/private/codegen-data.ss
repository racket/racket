#lang scheme/base
(require scheme/match
         (for-template scheme/base))
(provide (all-defined-out))

;; A PK is (make-pk (listof Pattern) stx)
;; k is the rhs expression:
;;   - open term with the attr names as free variables
;;   - attr name must be bound to variable of (listof^depth value)
;;   - 'fail' stxparameterized to (non-escaping!) failure procedure
(define-struct pk (ps k) #:transparent)

;; An ExtPK is one of
;;   - PK
;;   - (make-idpks stxclass (listof stx) (listof PK))
;;   - (make-cpks (listof PK) (listof DatumPKS) (listof LiteralPKS))
;;       the first field has only pair patterns
(define-struct idpks (stxclass args idpks))
(define-struct cpks (pairpks datumpks literalpks))

;; A DatumPKS is (make-datumpks datum (listof PK))
(define-struct datumpks (datum pks))

;; A LiteralPKS is (make-literalpks identifier (listof PK))
(define-struct literalpks (literal pks))


;; A FrontierContextExpr (FCE) is one of
;;   - (list  FrontierIndexExpr Syntax)
;;   - (list* FrontierIndexExpr Syntax FrontierContextExpr)
;; A FrontierIndexExpr is
;;   - `(+ ,Number Syntax ...)

(define (empty-frontier x)
  (list '(+ 0) x))

(define (done-frontier x)
  (list '(+ +inf.0) x))

(define (frontier:add-car fc x)
  (list* '(+ 0) x fc))

(define (frontier:add-cdr fc)
  (cons (fi:add1 (car fc))
        (cdr fc)))
(define (fi:add1 fi)
  `(+ ,(add1 (cadr fi)) ,@(cddr fi)))

(define (frontier:add-index fc expr)
  (cons (fi:add-index (car fc) expr)
        (cdr fc)))
(define (fi:add-index fi expr)
  `(+ ,(cadr fi) ,expr ,@(cddr fi)))

;; A DynamicFrontierContext (DFC) is one of
;;   - (list  Syntax Number)
;;   - (list* Syntax Number DynamicFrontierContext)

(define (frontier->expr fc)
  #`(list #,@(reverse fc)))
