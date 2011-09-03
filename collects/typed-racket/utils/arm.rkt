#lang racket/base
(require (for-template racket/base))

(provide arm)

;; For simplicity, protect everything produced by Typed Racket.
(define (arm stx)
  (syntax-case stx (module #%plain-module-begin
                       #%require #%provide begin
                       define-values define-syntaxes
                       define-values-for-syntax)
    [(module name initial-import mb)
     (quasisyntax/loc stx (module name initial-import #,(arm #'mb)))]
    [(#%plain-module-begin . _) (syntax-arm stx)]
    [(#%require . _) stx]
    [(#%provide . _) stx]
    [(begin form ...)
     (quasisyntax/loc stx (begin #,@(map arm (syntax->list #'(form ...)))))]
    [(define-values ids expr)
     (quasisyntax/loc stx (define-values ids #,(arm #'expr)))]
    [(define-syntaxes ids expr)
     (quasisyntax/loc stx (define-syntaxes ids #,(arm #'expr)))]
    [(define-values-for-syntax ids expr)
     (quasisyntax/loc stx (define-values-for-syntax ids #,(arm #'expr)))]
    [_ (syntax-arm stx)]))
