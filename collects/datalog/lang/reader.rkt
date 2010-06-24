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
           ; XXX Should have comment character key
           ; XXX repl submit
           (case key
             [(color-lexer)
              (dynamic-require `datalog/tool/syntax-color) 'get-syntax-token)]
             [else (default key defval)]))
  (require "../parse.rkt"))