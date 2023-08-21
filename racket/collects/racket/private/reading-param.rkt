(module reading-params '#%kernel
  (#%declare #:cross-phase-persistent)
  (#%require '#%paramz)
  (#%provide call-with-default-reading-parameterization)
  
  (define-values (call-with-default-reading-parameterization)
    (lambda (thunk)
      (if (if (procedure? thunk) (procedure-arity-includes? thunk 0) #f)
      (with-continuation-mark
        parameterization-key
        (extend-parameterization
         (continuation-mark-set-first '#f parameterization-key)
         read-case-sensitive #t
         read-square-bracket-as-paren #t
         read-curly-brace-as-paren #t
         read-square-bracket-with-tag #f
         read-curly-brace-with-tag #f
         read-accept-box #t
         read-accept-compiled #f
         read-accept-bar-quote #t
         read-accept-graph #t
         read-syntax-accept-graph #f
         read-decimal-as-inexact #t
         read-single-flonum #f
         read-cdot #f
         read-accept-dot #t
         read-accept-infix-dot #t
         read-accept-quasiquote #t
         read-accept-reader #f
         read-accept-lang #t
         current-readtable #f)
        (thunk))
      (raise-argument-error 'call-with-default-reading-parameterization
                            "(procedure-arity-includes/c 0)"
                            thunk)))))
