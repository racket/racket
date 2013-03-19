#lang racket/base

(require "../utils/utils.rkt"
         (for-template racket/base)
         (rep type-rep)
         syntax/parse
         unstable/debug
         syntax/id-table
         racket/contract
         racket/match
         racket/list
         racket/dict)

(provide register-scoped-tvars lookup-scoped-tvars
         add-scoped-tvars lookup-scoped-tvar-layer)

(define tvar-stx-mapping (make-weak-hash))

(define (add-scoped-tvars stx vars)
  (match vars
    [(or #f (list)) stx]
    [else
      (define (add-vars stx)
        (hash-update! tvar-stx-mapping stx (lambda (old-vars) (cons vars old-vars)) null))
      (let loop ((stx stx))
        (syntax-parse stx
          #:literals (#%expression #%plain-lambda let-values case-lambda)
          [(#%expression e) (loop #'e)]
          [(~or (case-lambda formals . body) (#%plain-lambda formals . body))
           (add-vars stx)]
          [(let-values ([(f) fun]) . body)
           #:when (or (syntax-property stx 'kw-lambda)
                      (syntax-property stx 'opt-lambda))
           (add-vars #'fun)]
          [e (void)]))]))

(define (lookup-scoped-tvar-layer stx)
  (hash-ref tvar-stx-mapping stx null))

;; free-id-table from id -> (listof (listof (or/c (listof identifier?)
;;                                                (list (listof identifier?) identifier?))))
(define tvar-mapping (make-free-id-table))

(define (lookup-scoped-tvars id)
  (dict-ref tvar-mapping id #f))

(define (register-scoped-tvars id tvars)
  (dict-set! tvar-mapping id tvars))

