(module reader syntax/module-reader
  #:language 'datalog/lang/module
  #:read (lambda ([in (current-input-port)])
           (let ([ast (parse-program in)])
             (list `(#%module-begin ,@ast))))
  #:read-syntax (lambda ([source-name #f] [in (current-input-port)])
                  (let ([ast (parse-program in)])
                    (list `(#%module-begin ,@ast))))
  #:whole-body-readers? #t  
  #:info (lambda (key defval default)
           ; XXX Should have different comment character key
           (case key
             [(drracket:submit-predicate)
              repl-submit?]
             [(color-lexer)
              (dynamic-require `datalog/tool/syntax-color 'get-syntax-token)]
             [else (default key defval)]))
  (require datalog/parse
           datalog/tool/submit)
  
  ; XXX This is almost certainly wrong.
  (define (even-read src ip)
    (begin0
      (parameterize ([current-source-name src])
       (datum->syntax #f (parse-statement ip)))
      (current-read-interaction odd-read)))
  (define (odd-read src ip)
    (current-read-interaction even-read)
    eof)
    
  (current-read-interaction
   even-read))