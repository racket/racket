(module kernel '#%kernel
  (#%require (for-syntax '#%kernel))

  ;; this is duplicated from "racket/private/pre-base.rkt"
  ;; but i'm not sure that file should require this one
  (define-syntaxes (#%top-interaction)
    (lambda (stx)
      (if (eq? 'top-level (syntax-local-context))
          'ok
          (raise-syntax-error
           #f
           "not at top level"
           stx))
      (if (symbol? (syntax-e stx))
          (raise-syntax-error #f "bad syntax" stx)
          (datum->syntax stx (cdr (syntax-e stx)) stx stx))))

  (#%provide (all-from '#%kernel) #%top-interaction))
