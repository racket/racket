#lang racket/base
(require syntax/kerncase
         syntax/stx)
(provide head-local-expand-and-categorize-syntaxes
         categorize-expanded-syntaxes
         head-local-expand-syntaxes)

;; head-local-expand-syntaxes : syntax boolean boolean -> stxs ^ 6
;; Setting allow-def-after-expr? allows def/expr interleaving.
(define (head-local-expand-and-categorize-syntaxes x allow-def-after-expr?)
  (define estxs (head-local-expand-syntaxes x allow-def-after-expr?))
  (define-values (defs vdefs sdefs exprs)
    (categorize-expanded-syntaxes estxs))
  (values estxs estxs defs vdefs sdefs exprs))

;; categorize-expanded-syntaxes : (listof stx) -> stxs ^ 4
;; Split head-expanded stxs into
;;   definitions, values-definitions, syntaxes-definitions, exprs
;; (definitions include both values-definitions and syntaxes-definitions.)
(define (categorize-expanded-syntaxes estxs0)
  (let loop ([estxs estxs0] [defs null] [vdefs null] [sdefs null] [exprs null])
    (cond [(pair? estxs)
           (let ([ee (car estxs)])
             (syntax-case ee (begin define-values define-syntaxes)
               [(define-values . _)
                (loop (cdr estxs)
                      (cons ee defs)
                      (cons ee vdefs)
                      sdefs
                      exprs)]
               [(define-syntaxes (var ...) rhs)
                (loop (cdr estxs)
                      (cons ee defs)
                      vdefs
                      (cons ee sdefs)
                      exprs)]
               [_
                (loop (cdr estxs)
                      defs
                      vdefs
                      sdefs
                      (cons ee exprs))]))]
          [(null? estxs)
           (values (reverse defs)
                   (reverse vdefs)
                   (reverse sdefs)
                   (reverse exprs))])))

;; head-local-expand-syntaxes : syntax boolean -> (listof syntax)
(define (head-local-expand-syntaxes x allow-def-after-expr?)
  (let ([intdef (syntax-local-make-definition-context)]
        [ctx '(block)])
    (let loop ([x x] [ex null] [expr? #f])
      (cond [(stx-pair? x)
             (let ([ee (local-expand (stx-car x)
                                     ctx
                                     (kernel-form-identifier-list)
                                     intdef)])
               (syntax-case ee (begin define-values define-syntaxes)
                 [(begin e ...)
                  (loop (append (syntax->list #'(e ...)) (stx-cdr x)) ex expr?)]
                 [(begin . _)
                  (raise-syntax-error #f "bad begin form" ee)]
                 [(define-values (var ...) rhs)
                  (andmap identifier? (syntax->list #'(var ...)))
                  (begin
                    (when (and expr? (not allow-def-after-expr?))
                      (raise-syntax-error #f "definition after expression" ee))
                    (syntax-local-bind-syntaxes (syntax->list #'(var ...)) #f intdef)
                    (loop (stx-cdr x) (cons ee ex) expr?))]
                 [(define-values . _)
                  (raise-syntax-error #f "bad define-values form" ee)]
                 [(define-syntaxes (var ...) rhs)
                  (andmap identifier? (syntax->list #'(var ...)))
                  (begin
                    (when (and expr? (not allow-def-after-expr?))
                      (raise-syntax-error #f "definition after expression" ee))
                    (syntax-local-bind-syntaxes (syntax->list #'(var ...))
                                                #'rhs
                                                intdef)
                    (loop (stx-cdr x) (cons ee ex) expr?))]
                 [(define-syntaxes . _)
                  (raise-syntax-error #f "bad define-syntaxes form" ee)]
                 [_
                  (loop (stx-cdr x) (cons ee ex) #t)]))]
            [(stx-null? x)
             (internal-definition-context-seal intdef)
             (reverse ex)]))))
