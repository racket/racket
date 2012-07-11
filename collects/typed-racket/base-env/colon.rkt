#lang racket/base

(require (for-syntax racket/base syntax/parse "internal.rkt" "../utils/disappeared-use.rkt")
         "../typecheck/internal-forms.rkt"
         (prefix-in t: "base-types-extra.rkt"))

(provide :)

(define-syntax (: stx)
  (define stx*
    ;; make it possible to add another colon after the id for clarity
    ;; and in that case, a `->' on the RHS does not need to be
    ;; explicitly parenthesized
    (syntax-parse stx #:literals (: t:->)
      [(: id (~and kw :) x ...)
       #:fail-unless (for/first ([i (syntax->list #'(x ...))]
                                 #:when (identifier? i)
                                 #:when (free-identifier=? i #'t:->))
                       i) 
       #f
       (add-disappeared-use #'kw)
       (syntax/loc stx (: id (x ...)))]
      [(: id : . more)
       (syntax/loc stx (: id . more))]
      [_ stx]))
  (define (err str . sub)
    (apply raise-syntax-error '|type declaration| str stx sub))
  (syntax-parse stx*
    [_
     #:when (eq? 'expression (syntax-local-context))
     (err "must be used in a definition context")]
    [(_ i:id ty)
     (syntax-property (internal (syntax/loc stx (:-internal i ty)))
                      'disappeared-use #'i)]
    [(_ i:id x ...)
     (case (length (syntax->list #'(x ...)))
       [(1)  (err "can only annotate identifiers with types" #'i)]
       [(0)  (err "missing type")]
       [else (err "bad syntax (multiple types after identifier)")])]))
