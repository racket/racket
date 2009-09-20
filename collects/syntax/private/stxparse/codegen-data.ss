#lang scheme/base
(require scheme/match
         syntax/stx
         (for-template scheme/base
                       syntax/stx
                       scheme/stxparam
                       "runtime.ss"))
(provide (all-defined-out))

;; Frontiers

;; A FrontierContextExpr (FCE) is one of
;;  - (make-fce Id (listof FrontierIndexExpr))
;; A FrontierIndexExpr is
;;  - #'(+ Number expr ...)
(define-struct fce (stx indexes) #:prefab)

(define (empty-frontier x)
  (make-fce x (list #'(+ 0))))

(define (done-frontier x)
  (make-fce x (list #'(+ 0) #'(+ +inf.0))))

(define (frontier:add-car fc x)
  (make-fce x (cons #'(+ 0) (fce-indexes fc))))

(define (frontier:add-cdr fc)
  (define (fi:add1 fi)
    (syntax-case fi (+)
      [(+ n . rest)
       #`(+ #,(add1 (syntax-e #'n)) . rest)]))
  (make-fce (fce-stx fc)
            (cons (fi:add1 (stx-car (fce-indexes fc)))
                  (stx-cdr (fce-indexes fc)))))

(define (frontier:add-index fc expr)
  (define (fi:add-index fi expr)
    (syntax-case fi (+)
      [(+ n . rest)
       #`(+ n #,expr . rest)]))
  (make-fce (fce-stx fc)
            (cons (fi:add-index (stx-car (fce-indexes fc)) expr)
                  (stx-cdr (fce-indexes fc)))))

(define (frontier:add-unvector fc x)
  (frontier:add-car fc x))
(define (frontier:add-unbox fc x)
  (frontier:add-car fc x))
(define (frontier:add-unpstruct fc x)
  (frontier:add-car fc x))

(define (frontier:add-subparse fc x)
  (frontier:add-car
   (frontier:add-index (frontier:add-car fc x) +inf.0)
   x))

;; A DynamicFrontierContext (DFC) is a list of numbers.
;; More operations on DFCs in runtime.ss

(define (frontier->dfc-expr fc)
  (define (fi->qq-part fi)
    (syntax-case fi (+)
      [(+ n)
       #'n]
      [expr #`(unquote expr)]))
  (let ([fis (reverse (stx->list (fce-indexes fc)))])
    (with-syntax ([(part ...) (map fi->qq-part fis)])
      #`(quasiquote (part ...)))))

(define (frontier->fstx-expr fc)
  (fce-stx fc))

(define (frontier->index-expr fc)
  (match fc
    [(struct fce (stx indexes))
     #`#,(stx-car indexes)]))

;; --------


(define (get-kind kind)
  (syntax-case kind ()
    [#:pair pairK]
    [#:vector vectorK]
    [#:box boxK]
    [(#:pstruct key)
     (make-kind #`(lambda (x)
                    (let ([xkey (prefab-struct-key x)])
                      (and xkey (equal? xkey (quote key)))))
                (list (lambda (s d)
                        #`(datum->syntax #,s (cdr (vector->list (struct->vector #,d))) #,s)))
                (list (lambda (fc x)
                        (frontier:add-unpstruct fc x))))]))

;; A Kind is
;;   (make-kind id (listof (id id -> stx)) (listof (FCE id -> FCE)))

(define-struct kind (predicate selectors frontier-procs) #:transparent)

(define pairK
  (make-kind #'pair?
             (list (lambda (s d) #`(car #,d))
                   (lambda (s d) #`(datum->syntax #,s (cdr #,d) #,s)))
             (list (lambda (fc x) (frontier:add-car fc x))
                   (lambda (fc x) (frontier:add-cdr fc)))))

(define vectorK
  (make-kind #'vector?
             (list (lambda (s d)
                     #`(datum->syntax #,s (vector->list #,d) #,s)))
             (list (lambda (fc x) (frontier:add-unvector fc x)))))

(define boxK
  (make-kind #'box?
             (list (lambda (s d) #`(unbox #,d)))
             (list (lambda (fc x) (frontier:add-unbox fc x)))))
