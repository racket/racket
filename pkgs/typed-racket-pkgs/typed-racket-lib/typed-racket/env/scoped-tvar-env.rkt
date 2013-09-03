#lang racket/base

;; Maintain mapping of type variables introduced by literal Alls in type annotations.

(require "../utils/utils.rkt"
         (for-template racket/base)
         syntax/parse
         syntax/id-table
         racket/match
         racket/dict)

(provide register-scoped-tvars lookup-scoped-tvars
         add-scoped-tvars lookup-scoped-tvar-layer)

;; tvar-stx-mapping: (hash/c syntax? (listof (listof identifier?)))
(define tvar-stx-mapping (make-weak-hash))

;; add-scoped-tvars: syntax? (or/c #f (listof identifier)) -> void?
;; Annotate the given expression with the given identifiers if it is safe.
;; If there are no identifiers, then nothing is done.
;; Safe expressions are lambda, case-lambda, or the expansion of keyword and opt-lambda forms.
(define (add-scoped-tvars stx vars)
  (match vars
    [(or #f (list)) (void)]
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

;; lookup-scoped-tvar-layer: syntax? -> (listof (listof identifier?))
;; Returns the identifiers associated with a given syntax object.
;; There can be multiple sections of identifiers, which correspond to multiple poly types.
(define (lookup-scoped-tvar-layer stx)
  (hash-ref tvar-stx-mapping stx null))

;; tvar-annotation? := (listof (listof (or/c (listof identifier?)
;;                                           (list (listof identifier?) identifier?))))
;; tvar-mapping: (free-id-table/c tvar-annotation?)
;; Keeps track of type variables that should be introduced when type checking
;; the definition for an identifier.
(define tvar-mapping (make-free-id-table))

;; lookup-scoped-tvars: identifier -> (or/c #f tvar-annotation?)
;; Lookup an indentifier in the scoped tvar-mapping.
(define (lookup-scoped-tvars id)
  (dict-ref tvar-mapping id #f))

;; Register type variables for an indentifier in the scoped tvar-mapping.
;; register-scoped-tvars: identifier? tvar-annotation? -> void?
(define (register-scoped-tvars id tvars)
  (dict-set! tvar-mapping id tvars))

