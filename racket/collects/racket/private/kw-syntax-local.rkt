(module kw-syntax-local "pre-base.rkt"
  (require (prefix-in k: '#%kernel))

  (provide syntax-local-binding syntax-local-value)

  (define (syntax-local-binding id [failure-thunk #f] [intdef-ctx '()] #:immediate? [immediate? #f])
    (k:syntax-local-binding id failure-thunk intdef-ctx immediate?))

  (define (syntax-local-value id [failure-thunk #f] [intdef-ctx '()] #:immediate? [immediate? #f])
    (k:syntax-local-value id failure-thunk intdef-ctx immediate?)))
