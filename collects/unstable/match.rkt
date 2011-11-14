#lang racket/base

(require racket/class
         racket/match
         (for-syntax racket/base)
         (for-syntax syntax/parse))

(provide match? as object)

(define-syntax-rule (match? e p ...)
  (match e [p #t] ... [_ #f]))

(define-match-expander as
  (syntax-rules ()
    [(as ([x e] ...) p ...) (and (app (lambda (y) e) x) ... p ...)]))

;; Added by asumu
;; Match expander for objects from racket/class
(define-match-expander object
  (λ (stx)
    (define-syntax-class field
      #:attributes (name pat)
      (pattern
        ((~datum field)
         name
         (~optional pat #:defaults ([pat #'name])))))

    (syntax-parse stx
      [(object f:field ...)
       #'(and (? object?)
              (and (? (λ (o) (field-bound? f.name o)))
                   (app (λ (o) (get-field f.name o))
                        f.pat))
              ...)]
      [(object class f:field ...)
       #'(and (? (λ (o) (is-a? o class)))
              (object f ...))])))
