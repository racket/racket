(module intdef '#%kernel
  (#%provide internal-definition-context-track)

  (define-values (internal-definition-context-track)
    (lambda (intdef stx)
      (if (internal-definition-context? intdef)
          (if (syntax? stx)
              (let-values ([(ids) (internal-definition-context-binding-identifiers intdef)])
                (if (null? ids)
                    stx
                    (let-values ([(v) (syntax-property stx 'disappeared-binding)])
                      (syntax-property stx 'disappeared-binding (if v (cons ids v) ids)))))
              (raise-argument-error 'internal-definition-context-track "syntax?" 1 intdef stx))
          (raise-argument-error 'internal-definition-context-track "internal-definition-context?" 0 intdef stx)))))
