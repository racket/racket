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

  (define (check-module-form exp expected-module filename)
    (cond [(or (eof-object? exp) (eof-object? (syntax-e exp)))
           (and filename
                (error 'load-handler
                       "expected a `module' declaration in ~s, but found end-of-file"
                       filename))]
          [(compiled-module-expression? (syntax-e exp))
           exp]
          [(and (syntax? exp)
                (syntax-case exp ()
                  [(mod nm . _)
                   (and (eq? (syntax-e #'mod) 'module) (identifier? #'nm))]
                  [_else #f]))
           ;; It's ok; need to install a specific `module' binding:
           (with-syntax ([(mod nm . _) exp])
             (datum->syntax exp
                            (cons (namespace-module-identifier)
                                  (cdr (syntax-e exp)))
                            exp
                            exp))]
          [else
           (and filename
                (error 'load-handler
                       "expected a `module' declaration in ~s, but found something else"
                       filename))])))
