#lang scheme/base

;; This module exists for documentaiton purposes: the
;; for-syntax exports of `rnrs/base-6' are exported
;; from here in phase 0

(require r6rs/private/identifier-syntax
         (for-syntax scheme/base
                     "check-pattern.rkt"))

(provide (rename-out [r6rs:syntax-rules syntax-rules])
         identifier-syntax
         ...
         _)

(define-syntax (r6rs:syntax-rules stx)
  (syntax-case stx ()
    [(_ (lit ...) [pat tmpl] ...)
     (let ([lits (syntax->list #'(lit ...))])
       (for-each
        (lambda (lit)
          (unless (identifier? lit)
            (raise-syntax-error #f
                                "literal is not an identifier"
                                stx
                                lit))
          (when (or (free-identifier=? lit (quote-syntax ...))
                    (free-identifier=? lit #'_))
            (raise-syntax-error #f
                                "not allowed as a literal"
                                stx
                                lit)))
        lits)
       (for-each (check-pat-ellipses stx) (syntax->list #'(pat ...)))
       (syntax-case stx ()
         [(_ . rest)
          (syntax/loc stx (syntax-rules . rest))]))]
    [(_ . rest)
     (syntax/loc stx (syntax-rules . rest))]))
