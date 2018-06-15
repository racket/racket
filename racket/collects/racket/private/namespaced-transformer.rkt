(module namespaced-transformer "pre-base.rkt"

  (require (for-syntax "pre-base.rkt"
                       "stx.rkt"
                       "stxcase-scheme.rkt"))

  (provide #%namespaced)

  (define-for-syntax (strip-context e)
    (cond
      [(syntax? e)
       (datum->syntax #f (strip-context (syntax-e e)) e e)]
      [(pair? e) (cons (strip-context (car e))
                       (strip-context (cdr e)))]
      [(vector? e) (list->vector
                    (map (lambda (e) (strip-context e))
                         (vector->list e)))]
      [(box? e) (box (strip-context (unbox e)))]
      [(prefab-struct-key e)
       => (lambda (k)
            (apply make-prefab-struct k
                   (strip-context (vector->list (cdr (struct->vector e))))))]
      [else e]))

  (define-syntax (#%namespaced stx)
    (syntax-case stx ()
      [(_ module-path form)
       (unless (module-path? (syntax->datum #'module-path))
         (raise-syntax-error #f "bad module path" stx #'module-path))
       (cond
         [(identifier? #'form)
          (syntax-local-introduce
           (syntax-local-lift-require
            (syntax-local-introduce (strip-context #'module-path))
            (syntax-local-introduce (strip-context #'form))))]
         [(stx-pair? #'form)
          (unless (identifier? (stx-car #'form))
            (raise-syntax-error #f "namespaced form must start with identifier" stx (stx-car #'form)))
          (datum->syntax #'form
                    (cons (syntax-local-introduce
                           (syntax-local-lift-require
                            (syntax-local-introduce (strip-context #'module-path))
                            (syntax-local-introduce (strip-context (stx-car #'form)))))
                          (stx-cdr #'form))
                    #'form #'form)]
         [else
          (raise-syntax-error #f "namespaced form must be pair or identifier" stx #'form)])]
      [(_ _)
       (raise-syntax-error #f "missing namespaced form" stx)]
      [(_)
       (raise-syntax-error #f "missing module path and namespaced form" stx)]
      [(_ _ _ . _)
       (raise-syntax-error #f "too many sub-forms" stx)])))