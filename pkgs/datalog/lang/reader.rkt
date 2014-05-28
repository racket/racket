(module reader syntax/module-reader
  #:language 'datalog/sexp/lang
  #:read (lambda ([in (current-input-port)]) (this-read-syntax #f in))
  #:read-syntax this-read-syntax
  #:whole-body-readers? #t  
  #:language-info
  '#(datalog/lang/lang-info get-info #f)
  #:info (lambda (key defval default)
           ; XXX Should have different comment character key
           (case key
             [(drracket:default-filters) '(["Datalog Sources" "*.dl"])]
             [(drracket:default-extension) "dl"]
             [(drracket:submit-predicate)
              (dynamic-require 'datalog/tool/submit 'repl-submit?)]
             [(color-lexer)
              (dynamic-require 'datalog/tool/syntax-color 'get-syntax-token)]
             [else (default key defval)]))
  (require datalog/parse
           datalog/private/compiler)
  
  (define (this-read-syntax [src #f] [in (current-input-port)])
    (compile-program
     (parameterize ([current-source-name src])
       (parse-program in)))))
