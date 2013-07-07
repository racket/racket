(module modread racket/base
  (require racket/contract/base)
  
  (provide with-module-reading-parameterization)
  (provide/contract
   [check-module-form ((or/c syntax? eof-object?) (or/c symbol? list?) (or/c string? path? false/c) . -> . any)])

  (define (with-module-reading-parameterization thunk)
    (call-with-default-reading-parameterization
     (lambda ()
       (parameterize ([read-accept-reader #t]
                      [read-accept-lang #t]
                      [read-accept-compiled #t])
         (thunk)))))

  (define (raise-wrong-module-name filename expected-name name)
    (error 'load-handler
           "expected a `module' declaration for `~a' in ~s, found: ~a"
           expected-name filename name))

  (define (check-module-form exp expected-module filename)
    (cond [(or (eof-object? exp) (eof-object? (syntax-e exp)))
           (and filename
                (error 'load-handler
                       "expected a `module' declaration for `~a' in ~s, but found end-of-file"
                       expected-module filename))]
          [(compiled-module-expression? (syntax-e exp))
           (if (or #t ; we don't check the name anymore
                   (eq? (module-compiled-name (syntax-e exp)) expected-module))
             ;; It's fine:
             exp
             ;; Wrong name:
             (and filename (raise-wrong-module-name
                            filename expected-module
                            (module-compiled-name (syntax-e exp)))))]
          [(and (syntax? exp)
                (syntax-case exp ()
                  [(mod nm . _)
                   (and (eq? (syntax-e #'mod) 'module) (identifier? #'nm))]
                  [_else #f]))
           ;; It's ok; need to install a specific `module' binding:
           (with-syntax ([(mod nm . _) exp])
             (when #f ; we don't check the name anymore
               (unless (eq? (syntax-e #'nm) expected-module)
                 (raise-wrong-module-name filename expected-module
                                          (syntax-e #'nm))))
             (datum->syntax exp
                                   (cons (namespace-module-identifier)
                                         (cdr (syntax-e exp)))
                                   exp
                                   exp))]
          [else
           (and filename
                (error 'load-handler
                       "expected a `module' declaration for `~a' in ~s, but found something else"
                       expected-module filename))])))
