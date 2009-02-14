#lang scheme/base
(require scheme/match
         (for-template scheme/base "runtime.ss"))
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
;;  - (make-fce Id FrontierIndexExpr)
;;  - (make-joined-frontier FCE id)
;; A FrontierIndexExpr is
;;  - `(+ ,Number ,Syntax ...)
(define-struct fce (stx indexes))
(define-struct joined-frontier (base ext) #:transparent)

(define (empty-frontier x)
  (make-fce x (list '(+ 0))))

(define (done-frontier x)
  (make-fce x (list '(+ +inf.0))))

(define (frontier:add-car fc x)
  (make-fce x (cons '(+ 0) (fce-indexes fc))))

(define (frontier:add-cdr fc)
  (define (fi:add1 fi)
    `(+ ,(add1 (cadr fi)) ,@(cddr fi)))
  (make-fce (fce-stx fc)
            (cons (fi:add1 (car (fce-indexes fc)))
                  (cdr (fce-indexes fc)))))

(define (frontier:add-index fc expr)
  (define (fi:add-index fi expr)
    `(+ ,(cadr fi) ,expr ,@(cddr fi)))
  (make-fce (fce-stx fc)
            (cons (fi:add-index (car (fce-indexes fc)) expr)
                  (cdr (fce-indexes fc)))))

(define (join-frontiers base ext)
  (make-joined-frontier base ext))

;; A DynamicFrontierContext (DFC) is a list of numbers.
;; More operations on DFCs in runtime.ss

(define (frontier->dfc-expr fc)
  (define (loop fc)
    (match fc
      [(struct fce (stx indexes))
       #`(list #,@indexes)]
      [(struct joined-frontier (base ext))
       #`(let ([base #,(loop base)])
           (if (failed? #,ext)
               (append (reverse (failed-frontier #,ext)) base)
               base))]))
  #`(reverse #,(loop fc)))

(define (frontier->fstx-expr fc)
  (define (loop fc)
    (match fc
      [(struct fce (stx indexes))
       stx]
      [(struct joined-frontier (base ext))
       #`(let ([inner-failure #,ext])
           (or (and (failed? inner-failure) (failed-frontier-stx inner-failure))
               #,(loop base)))]))
  (loop fc))
