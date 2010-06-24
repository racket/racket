(module reader syntax/module-reader
  #:language `(planet ,(this-package-version-symbol lang/module))
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
              (dynamic-require `(planet ,(this-package-version-symbol drscheme/syntax-color)) 'get-syntax-token)]
             [else (default key defval)]))
  (require (planet cce/scheme:6/planet)
           "../parse.ss"))