
;;----------------------------------------------------------------------
;; #%define : define and define-syntax

(module define '#%kernel
  (#%require (for-syntax '#%kernel "stx.rkt")
             "define-et-al.rkt")

  (#%provide define 
             define-syntax 
             define-values-for-syntax
             define-for-syntax)

  (define-syntaxes (define-values-for-syntax)
    (lambda (stx)
      (raise-syntax-error-if (identifier? stx) "bad syntax" stx)
      (define-values (lst) (syntax->list stx))
      (raise-syntax-error-unless lst "bad syntax (illegal use of `.')" stx)
      (raise-syntax-error-if (null? (cdr lst)) "bad syntax (missing names to define)" stx)
      (define-values (ids) (syntax->list (cadr lst)))
      (raise-syntax-error-unless ids
                                 "bad syntax (expected a list of identifiers)"
                                 stx
                                 (cadr lst))
      (for-each (lambda (id)
                  (raise-syntax-error-unless (identifier? id) "not an identifier" stx id))
                ids)
      ; TODO(jsailor): April 2026-ish: uncomment when raise-if-duplicate-identifiers is committed
      ;(raise-if-duplicate-identifiers "duplicate identifier" stx ids)
      (raise-syntax-error-if (null? (cddr lst)) "bad syntax (missing expression after identifiers)" stx)
      (raise-syntax-error-if (pair? (cdddr lst)) "bad syntax (multiple expressions after identifiers)" stx)
      (datum->syntax #f
                     (list (quote-syntax begin-for-syntax)
                           (datum->syntax #f
                                          (list* (quote-syntax define-values) (cdr lst))
                                          stx))
                     stx))))
