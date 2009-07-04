#lang scheme
(require scheme/stxparam)

(define-syntax-parameter bidi-match-going-in? #t)

(define-syntax (define-bidi-match-expander stx)
  (syntax-case stx ()
    [(_ bidi-id in-expander out-expander)
     (syntax/loc stx
       (define-match-expander bidi-id
         (lambda (stx)
           (syntax-case stx ()
             [(_ id)
              (if (syntax-parameter-value #'bidi-match-going-in?)
                  (syntax/loc stx (in-expander id))
                  (syntax/loc stx (out-expander id)))]))))]))

(provide bidi-match-going-in?
         define-bidi-match-expander)
