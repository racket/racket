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

;; A Group (G) is one of
;;   - PK
;;   - (make-idG stxclass (listof stx) (listof PK))
;;     where each PK starts with an id pattern of given stxclass/args
;;   - (make-descrimG (listof DatumSG) (listof LiteralSG) (listof CompountSGs))
;;     where each DatumSG/LiteralSG/CompoundSG has a different datum/lit/kind
(define-struct idG (stxclass args idpks) #:transparent)
(define-struct descrimG (datumSGs literalSGs kindSGs) #:transparent)

;; A DatumSG is (make-datumSG datum (listof PK))
;; where each PK starts with a datum pattern equal to datum
(define-struct datumSG (datum pks))

;; A LiteralSG is (make-literalSG id (listof PK))
;; where each PK starts with a literal pattern equal to literal
(define-struct literalSG (literal pks))

;; A CompoundSG is (make-compoundSG Kind (listof PK))
;; where each PK starts with a compound pattern of given kind
(define-struct compoundSG (kind pks))


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

(define (frontier:add-unvector fc)
  (frontier:add-car fc (fce-stx fc)))
(define (frontier:add-unbox fc)
  (frontier:add-car fc (fce-stx fc)))

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
           (or (and (failed? inner-failure)
                    (failed-frontier-stx inner-failure))
               #,(loop base)))]))
  (loop fc))
