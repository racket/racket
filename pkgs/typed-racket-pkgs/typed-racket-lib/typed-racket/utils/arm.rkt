#lang racket/base
(require (for-template racket/base) syntax/stx)

(provide arm)

;; For simplicity, protect everything produced by Typed Racket.
(define (arm stx)
  (syntax-case stx (#%plain-module-begin
                    #%require #%provide #%declare begin
                    define-values define-syntaxes
                    begin-for-syntax)
    [(#%plain-module-begin . _) (syntax-property (syntax-arm stx)
                                                 'taint-mode
                                                 'opaque)]
    [(#%require . _) stx]
    [(#%provide . _) stx]
    [(#%declare . _) stx]
    [(begin form ...)
     (quasisyntax/loc stx (begin #,@(stx-map arm #'(form ...))))]
    [(begin-for-syntax form ...)
     (quasisyntax/loc stx (begin-for-syntax #,@(stx-map arm #'(form ...))))]
    [(define-values ids expr)
     (quasisyntax/loc stx (define-values ids #,(arm #'expr)))]
    [(define-syntaxes ids expr)
     (quasisyntax/loc stx (define-syntaxes ids #,(arm #'expr)))]
    [_ (syntax-arm stx)]))
