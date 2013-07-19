(module pre-base '#%kernel
  (#%require (for-syntax '#%kernel))

  (#%provide #%top-interaction)

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
          (datum->syntax stx (cdr (syntax-e stx)) stx stx)))))

