
(module old-procs '#%kernel
  (#%require "small-scheme.ss"
             "more-scheme.ss"
             "misc.ss"
             "stxmz-body.ss"
             "define.ss")

  (#%provide make-namespace
             free-identifier=?*
             namespace-transformer-require)

  (define reflect-var #f)
  
  (define make-namespace
    (case-lambda
     [() (make-namespace 'initial)]
     [(flag)
      (unless (memq flag '(initial empty))
        (raise-syntax-error 'make-namespace
                            "'initial or 'empty"
                            flag))
      (let ([new (make-empty-namespace)]
            [old (variable-reference->empty-namespace (#%variable-reference reflect-var))])
        (namespace-attach-module old 'mzscheme new)
        (parameterize ([current-namespace new])
          (namespace-require/copy 'mzscheme))
        new)]))

  (define (free-identifier=?* a b)
    (and (eq? (syntax-e a)
              (syntax-e b))
         (free-identifier=? a b)))

  (define (namespace-transformer-require qrs)
    (namespace-require `(for-syntax ,qrs))))
