#lang racket/base

(require (for-syntax racket/base syntax/parse unstable/sequence unstable/syntax
                     "../utils/disappeared-use.rkt")
         "../typecheck/internal-forms.rkt"
         (prefix-in t: "base-types-extra.rkt"))

(provide :)

(begin-for-syntax
 (define (err str stx . sub)
   (apply raise-syntax-error '|type declaration| str stx sub))

 ;; Wrap the `:-expr` with two things:
 ;;   * (define-values () ...) to do what `internal` does
 ;;   * (#%expression ...) to delay the unbound id check
 (define (wrap stx :-expr)
   (quasisyntax/loc stx (define-values () (#%expression #,:-expr)))))

(define-syntax (: stx)
  ;; make it possible to add another colon after the id for clarity
  ;; and in that case, a `->' on the RHS does not need to be
  ;; explicitly parenthesized
  (syntax-parse stx #:literals (: t:->)
    [_
     #:when (eq? 'expression (syntax-local-context))
     (err stx "must be used in a definition context")]
    [(: id (~and kw :) x ...)
     #:fail-unless (for/first ([i (in-syntax #'(x ...))]
                               #:when (identifier? i)
                               #:when (free-identifier=? i #'t:->))
                     i)
     #f
     (add-disappeared-use #'kw)
     (wrap stx #`(:-helper #,(syntax-local-context) id (x ...)))]
    [(: id : . more)
     (wrap stx #`(:-helper #,(syntax-local-context) id . more))]
    [(: e ...) (wrap stx #`(:-helper #,(syntax-local-context) e ...))]))

(define-syntax (:-helper stx)
  (syntax-parse stx
    [(_ ctx i:id ty)
     (unless (or (eq? 'top-level (syntax-e #'ctx))
                 (identifier-binding #'i))
       (raise-syntax-error #f "unbound identifier in module" #'i))
     (syntax-property (syntax/loc stx (begin (quote-syntax (:-internal i ty))
                                             (#%plain-app values)))
                      'disappeared-use #'i)]
    [(_ i:id x ...)
     (case (syntax-length #'(x ...))
       [(1)  (err stx "can only annotate identifiers with types" #'i)]
       [(0)  (err stx "missing type")]
       [else (err stx "bad syntax (multiple types after identifier)")])]))

