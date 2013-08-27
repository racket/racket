(module reader racket/base
  (require syntax/module-reader)

  (provide (rename-out [-read read]
                       [-read-syntax read-syntax]
                       [-get-info get-info]))

  (define-values (-read -read-syntax -get-info)
    (make-meta-reader
     'reader
     "language path"
     #:read-spec (lambda (in) (read in))
     (lambda (s) (and (module-path? s) s))
     values
     values
     values)))
