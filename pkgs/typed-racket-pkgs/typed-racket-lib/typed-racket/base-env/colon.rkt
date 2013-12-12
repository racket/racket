#lang racket/base

(require (for-syntax racket/base syntax/parse unstable/sequence unstable/syntax
                     "../utils/disappeared-use.rkt"
                     (only-in "../utils/tc-utils.rkt" tc-error/stx))
         "../typecheck/internal-forms.rkt"
         (prefix-in t: "base-types-extra.rkt"))

(provide :)

(begin-for-syntax
 (define (err stx str . sub)
   (apply raise-syntax-error '|type declaration| str stx sub))

 ;; Wrap the `:-expr` with a `define-values`. This is like
 ;; what `internal` does, but the work is spread out among two
 ;; macros to delay the unbound identifier check.
 (define (wrap stx :-expr)
   (quasisyntax/loc stx (define-values () #,:-expr))))

(define-syntax (: stx)
  (define ctx (syntax-local-context))
  (define top-level? (eq? 'top-level ctx))
  ;; make it possible to add another colon after the id for clarity
  ;; and in that case, a `->' on the RHS does not need to be
  ;; explicitly parenthesized
  (syntax-parse stx #:literals (: t:->)
    [_
     #:when (eq? 'expression ctx)
     (err stx "must be used in a definition context")]
    [(: id (~and kw :) x ...)
     #:fail-unless (for/first ([i (in-syntax #'(x ...))]
                               #:when (identifier? i)
                               #:when (free-identifier=? i #'t:->))
                     i)
     #f
     (add-disappeared-use #'kw)
     (wrap stx #`(:-helper #,top-level? id (x ...)))]
    [(: id : . more)
     (wrap stx #`(:-helper #,top-level? id . more))]
    [(: e ...) (wrap stx #`(:-helper #,top-level? e ...))]))

(define-syntax (:-helper stx)
  (syntax-parse stx
    [(_ top-level? i:id ty)
     (unless (or (syntax-e #'top-level?)
                 (identifier-binding #'i))
       (tc-error/stx #'i
                     "Declaration for `~a' provided, but `~a' has no definition"
                     (syntax-e #'i)
                     (syntax-e #'i)))
     (syntax-property (syntax/loc stx (begin (quote-syntax (:-internal i ty))
                                             (#%plain-app values)))
                      'disappeared-use #'i)]
    [(_ _ i:id x ...)
     (case (syntax-length #'(x ...))
       [(1)  (err stx "can only annotate identifiers with types" #'i)]
       [(0)  (err stx "missing type")]
       [else (err stx "bad syntax (multiple types after identifier)")])]))

