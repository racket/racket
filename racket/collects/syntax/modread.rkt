(module modread racket/base
  (provide with-module-reading-parameterization
           check-module-form)

  (define (with-module-reading-parameterization thunk)
    (call-with-default-reading-parameterization
     (lambda ()
       (parameterize ([read-accept-reader #t]
                      [read-accept-lang #t]
                      [read-accept-compiled #t])
         (thunk)))))

  (define (check-module-form exp expected-module filename)
    (unless (or (syntax? exp) (eof-object? exp))
      (raise-argument-error 'check-module-form "(or/c syntax? eof-object?)" exp))
    (unless (or (symbol? expected-module) (list? expected-module))
      (raise-argument-error 'check-module-form "(or/c symbol? list?)" list))
    (unless (or (not filename) (path-string? filename))
      (raise-argument-error 'check-module-form "(or/c path-string? false/c)" list))

    (cond [(or (eof-object? exp) (eof-object? (syntax-e exp)))
           (and filename
                (error 'load-handler
                       "expected a `module` declaration in ~s, but found end-of-file"
                       filename))]
          [(compiled-module-expression? (syntax-e exp))
           exp]
          [(and (syntax? exp)
                (syntax-case exp ()
                  [(mod nm . _)
                   (and (eq? (syntax-e #'mod) 'module) (identifier? #'nm))]
                  [_else #f]))
           ;; It's ok; need to install a specific `module` binding:
           (with-syntax ([(mod nm . _) exp])
             (datum->syntax exp
                            (cons (namespace-module-identifier)
                                  (cdr (syntax-e exp)))
                            exp
                            exp))]
          [else
           (and filename
                (error 'load-handler
                       "expected a `module` declaration in ~s, but found something else"
                       filename))])))
