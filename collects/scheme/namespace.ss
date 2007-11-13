(module namespace scheme/base

  (provide make-base-namespace)

  (define orig-namespace
    (variable-reference-namespace (#%variable-reference orig-namespace)))      
  (define (make-base-namespace)
    (let ([ns (make-namespace)])
      (namespace-attach-module orig-namespace 'scheme/base ns)
      ns)))
