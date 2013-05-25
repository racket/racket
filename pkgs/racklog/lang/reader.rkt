(module reader syntax/module-reader
  ; XXX Copied shamelessly from datalog/lang/reader (some things should be better designed to share
  #:language 'racklog/lang/lang
  #:read (lambda ([in (current-input-port)]) (this-read-syntax #f in))
  #:read-syntax this-read-syntax
  #:whole-body-readers? #t  
  #:language-info
  '#(racklog/lang/lang-info get-info #f)
  #:info (lambda (key defval default)
           ; XXX Should have different comment character key
           (case key
             [(drracket:submit-predicate)
              (dynamic-require 'datalog/tool/submit 'repl-submit?)]
             [(color-lexer)
              (dynamic-require 'datalog/tool/syntax-color 'get-syntax-token)]
             [else (default key defval)]))
  (require datalog/parse
           racklog/lang/compiler)
  
  (define (this-read-syntax [src #f] [in (current-input-port)])
    (list
     (compile-program
      (parameterize ([current-source-name src])
        (parse-program in))))))
