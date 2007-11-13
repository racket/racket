
(module namespace-reflect scheme/base
  (require (for-syntax scheme/base))

  (provide define-reflection-anchor
           reflection-anchor->namespace)

  (define-syntax (define-reflection-anchor stx)
    (unless (memq (syntax-local-context) '(top-level module))
      (raise-syntax-error #f
                          "allowed only in a top-level or module context"
                          stx))
    (syntax-case stx ()
      [(_ id)
       (let ([id-stx #'id])
         (unless (identifier? id-stx)
           (raise-syntax-error #f
                               "expected an identifier"
                               stx
                               id-stx))
         #'(define id (make-reflection-anchor (#%variable-reference id))))]))

  (define-struct reflection-anchor (var))

  (define (reflection-anchor->namespace ra)
    (unless (reflection-anchor? ra)
      (raise-type-error 'reflection-anchor->namespace
                        "reflection anchor"
                        ra))
    (variable-reference-namespace (reflection-anchor-var ra))))
