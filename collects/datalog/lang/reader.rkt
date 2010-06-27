(module reader syntax/module-reader
  #:language 'datalog/sexp/lang
  #:read (lambda ([in (current-input-port)]) (this-read-syntax #f in))
  #:read-syntax this-read-syntax
  #:whole-body-readers? #t  
  #:info (lambda (key defval default)
           ; XXX Should have different comment character key
           (case key
             [(drracket:submit-predicate)
              (dynamic-require 'datalog/tool/submit 'repl-submit?)]
             [(color-lexer)
              (dynamic-require 'datalog/tool/syntax-color 'get-syntax-token)]
             [(configure-runtime)
              (λ ()
                (current-read-interaction even-read))]
             [else (default key defval)]))
  (require datalog/parse
           datalog/private/compiler)
  
  (define (this-read-syntax [src #f] [in (current-input-port)])
    (list
     (compile-program
      (parameterize ([current-source-name src])
        (parse-program in)))))
  
  ; XXX This is almost certainly wrong.
  (define (even-read src ip)
    (begin0
      (compile-statement
       (parameterize ([current-source-name src])
        (parse-statement ip)))
      (current-read-interaction odd-read)))
  (define (odd-read src ip)
    (current-read-interaction even-read)
    eof))