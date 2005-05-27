(module compile-lang-syntax mzscheme
  (require-for-syntax "compile-lang.ss")
  
  (provide compile-rest-of-lang)
  
  (define-syntax (compile-rest-of-lang stx)
    (syntax-case stx ()
      [(_ names) (compile-exceptions (syntax names))]))

  )