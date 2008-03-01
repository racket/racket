(module macro-utilities mzscheme
  (provide with-captures)
  
  (define-syntax (with-captures stx)
    (syntax-case stx ()
      [(_ so (name ...) body)
       #'(with-syntax
             ([name (datum->syntax-object so 'name)]
              ...)
           body)])))
