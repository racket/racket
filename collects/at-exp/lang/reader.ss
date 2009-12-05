(module reader scheme/base
  (require syntax/module-reader
           (only-in scribble/reader make-at-readtable))

  (provide (rename-out [at-read read]
                       [at-read-syntax read-syntax]
                       [at-get-info get-info]))

  (define at-readtable (make-at-readtable))

  (define (wrap-reader p)
    (lambda args
      (parameterize ([current-readtable at-readtable])
        (apply p args))))

  (define-values (at-read at-read-syntax at-get-info)
    (make-meta-reader
     'at-exp
     "language path"
     (lambda (str)
       (let ([s (string->symbol
                 (string-append (bytes->string/latin-1 str)
                                "/lang/reader"))])
         (and (module-path? s) s)))
     wrap-reader
     wrap-reader
     (lambda (proc)
       (lambda (key defval)
         (case key
           [(color-lexer)
            (dynamic-require 'syntax-color/scribble-lexer 'scribble-lexer)]
           [else (if proc (proc key defval) defval)]))))))
